
{-# language LambdaCase, Strict, TupleSections, CPP #-}
{-# options_ghc -Wincomplete-patterns -Wunused-imports #-}

module StrictImplParams (plugin) where

import Data.Foldable
import Data.Maybe
import GHC.Core.Class (classMethods)
import GHC.Core.Predicate
import GHC.Plugins

import qualified GHC.Core.TyCo.Rep as GHC

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = \_ todo -> pure (CoreDoPluginPass "Strict Implicit Params" pass : todo),
  pluginRecompile  = purePlugin
  }

{-# inline ($$!) #-}
($$!) :: (a -> b) -> a -> b
f $$! x = f x
infixl 8 $$!

{-# inline ($$~) #-}
($$~) :: (a -> b) -> a -> b
f $$~ ~x = f x
infixl 8 $$~

map' :: (a -> b) -> [a] -> [b]
map' f = foldr' (\a bs -> (:) $$! f a $$! bs) []
{-# inline map' #-}

#if __GLASGOW_HASKELL__ <= 904
manyType :: Mult
manyType = Many
#else
manyType :: Mult
manyType = ManyTy
#endif

#if __GLASGOW_HASKELL__ <= 906
coreFullView :: Type -> Type
coreFullView a = case coreView a of
  Just a -> coreFullView a
  _      -> a

isImplicitParamTy :: Type -> Bool
isImplicitParamTy ty = isJust $ isIPPred_maybe ty
#else
isImplicitParamTy :: Type -> Bool
isImplicitParamTy ty = isJust $ do
  (cls, tys) <- getClassPredTys_maybe ty
  isIPPred_maybe cls tys
#endif

-- | The value stored in an implicit parameter dictionary, together with its type,
--   i.e. @(ip \@x \@a d, a)@ for @d :: IP x a@.
ipValue :: Var -> Maybe (CoreExpr, Type)
ipValue x = do
  (tc, args) <- splitTyConApp_maybe (varType x)
  cls        <- tyConClass_maybe tc
  valTy      <- case args of [_sym, valTy] -> Just valTy; _ -> Nothing
  sel        <- case classMethods cls of [sel] -> Just sel; _ -> Nothing
  pure (mkCoreApps (Var sel) (foldr' (\a as -> (Type a:) $! as) [Var x] args), valTy)

-- | Force the value of an implicit param, continue with CoreExpr body that has Type
--   type.
--
--   Note: we force the *value* stored in the dictionary, by applying the class method
--   to it.
forceVar :: Var -> CoreExpr -> Type -> CoreExpr
forceVar x body bodyTy = case ipValue x of
  Just (val, valTy) -> mkWildCase val (GHC.Scaled manyType valTy) bodyTy [Alt DEFAULT [] body]
  Nothing           -> error "forceVar: not an implicit parameter dictionary"

pass :: ModGuts -> CoreM ModGuts
pass guts = do
  dflags <- getDynFlags

  let dbg :: Outputable a => a -> String
      dbg x = showSDoc dflags (ppr x)

  let goBind :: CoreBind -> CoreBind
      goBind b = let

        go :: [Var] -> CoreExpr -> Type -> CoreExpr
        go vars t a = case t of
          Lam x t -> case coreFullView a of
            -- Note: a Core type lambda binds its own type variable, which is not necessarily the
            -- one bound by the ForAllTy in the definition's type. Since we use the def type to
            -- generate the Core of the forcing, we need to rename the type binder to match the
            -- lambda binder. We sincerely hope that Core names are unique and this renaming doesn't
            -- introduce shadowing.
            GHC.ForAllTy bndr a -> let tv = binderVar bndr
                                       a' | isTyVar x, tv /= x = substTyWith [tv] [mkTyVarTy x] a
                                          | otherwise          = a
                                   in Lam x $! go vars t a'
            -- Precaution: we add an occurrence of the binder, so whatever occurrence info it
            -- carries from the desugarer should be outdated.
            GHC.FunTy _ _ a b | isImplicitParamTy a -> let x' = zapIdOccInfo x in
                                                       Lam x' $! go (x':vars) t b
                              | otherwise           -> Lam x $! go vars t b
            _ -> error $ "unexpected type for lambda expression: " ++ dbg a
          t ->
            foldl' (\acc x -> forceVar x acc a) (goExpr t) vars

        in case b of
          NonRec b t -> NonRec b $! go [] t (varType b)
          Rec defs   -> Rec $! map' (\(b, t) -> (b,) $! go [] t (varType b)) defs

      goExpr :: CoreExpr -> CoreExpr
      goExpr t = case t of
        Var{}                  -> t
        Lit{}                  -> t
        App t u                -> App $$! goExpr t $$! goExpr u
        Case t scr bodyty alts -> Case $$! goExpr t $$! scr $$! bodyty $$! map' goAlt alts
        Cast t coe             -> Cast $$! goExpr t $$~ coe
        Tick tck t             -> Tick tck $$! goExpr t
        Type{}                 -> t
        Coercion{}             -> t

        -- Note: the bound var of a free-standing lambda
        -- does not get forced!
        Lam x t -> Lam x $! goExpr t

        -- Neither does an implicit let binder!
        -- In both cases the issue is that we don't know the type
        -- of the expr body and I don't like the idea of recomputing it!
        Let b t -> Let $$! goBind b $$! goExpr t

      goAlt :: Alt CoreBndr -> Alt CoreBndr
      goAlt (Alt con bs body) = Alt con bs $! goExpr body

  let mg_binds' = map' goBind (mg_binds guts)
  pure $! guts {mg_binds = mg_binds'}
