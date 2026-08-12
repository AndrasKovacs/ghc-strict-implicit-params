
{-# language LambdaCase, Strict, TupleSections, CPP #-}
{-# options_ghc -Wincomplete-patterns -Wunused-imports #-}

module StrictImplParams (plugin) where

import Data.Foldable
import Data.Maybe
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

-- | Force var, continue with CoreExpr body that has Type type.
forceVar :: Var -> CoreExpr -> Type -> CoreExpr
forceVar x body bodyTy =
  mkWildCase (Var x) (GHC.Scaled manyType (varType x)) bodyTy [Alt DEFAULT [] body]

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
            GHC.ForAllTy _ a  -> Lam x $! go vars t a
            GHC.FunTy _ _ a b | isImplicitParamTy a -> Lam x $! go (x:vars) t b
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
