
{-# language
  LambdaCase,
  Strict,
  TemplateHaskell,
  TupleSections,
  ViewPatterns
  #-}

{-# options_ghc
  -Wincomplete-patterns
  -Wunused-imports
  #-}

module StrictImplParams (plugin) where

import System.Exit
import Data.Foldable

import GHC.Classes
import GHC.Plugins

import qualified Language.Haskell.TH as TH
import qualified GHC.Core.TyCo.Rep   as GHC

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = \_ todo ->
    pure (CoreDoPluginPass "Strict Implicit Params" pass : todo)
  }

fromTHName :: TH.Name -> CoreM Name
fromTHName thn = thNameToGhcName thn >>= \case
  Nothing -> do
    errorMsg $ text "Could not resolve TH name" <+> text (show thn)
    liftIO exitFailure
  Just n -> pure n

map' :: (a -> b) -> [a] -> [b]
map' f = foldr' (\a bs -> ((:) $! f a) $! bs) []
{-# inline map' #-}

forceVar :: Var -> CoreExpr -> Type -> CoreExpr
forceVar x u uty = Case (Var x) x uty [Alt DEFAULT [] u]

setNoOccInfo :: Var -> Var
setNoOccInfo x = case idInfo x of
  i -> lazySetIdInfo x (i {occInfo = noOccInfo})

pass :: ModGuts -> CoreM ModGuts
pass guts = do
  ipName <- fromTHName ''GHC.Classes.IP

  let goDef :: [Var] -> CoreExpr -> Type -> CoreExpr
      goDef xs t a = case t of
        Lam x t -> case a of
          GHC.ForAllTy _ a  -> Lam x $! goDef xs t a
          GHC.FunTy _ _ a b
            | Just (getName -> con, _) <- splitTyConApp_maybe a, con == ipName ->
                Lam x $! goDef (((:) $! setNoOccInfo x) xs) t b
            | otherwise ->
                Lam x $! goDef xs t b
          _ -> error "StrictImplParams: impossible"
        t -> foldl' (\acc x -> forceVar x acc a) t xs

  let goBind :: CoreBind -> CoreBind
      goBind = \case
        NonRec b t -> NonRec b $! goDef [] t (varType b)
        Rec defs   -> Rec $! map' (\(b, t) -> (b,) $! goDef [] t (varType b)) defs

  pure $! guts {mg_binds = map' goBind (mg_binds guts)}



-- pass :: ModGuts -> CoreM ModGuts
-- pass guts = do

--   dflags <- getDynFlags
--   -- seqId  <- GHC.lookupId =<< fromTHName 'GHC.Prim.seq
--   ipName <- fromTHName ''GHC.Classes.IP

--   let sh :: Outputable a => a -> String
--       sh = showPpr dflags

--   let ppr' :: CoreExpr -> String
--       ppr' = \case
--         Var x   -> sh x
--         App t u -> ppr' t ++ " " ++ "(" ++ ppr' u ++ ")"
--         t       -> sh t

--   let setNoOccInfo :: Var -> Var
--       setNoOccInfo x = case idInfo x of
--         i -> lazySetIdInfo x (i {occInfo = noOccInfo})

--   let force x u uty = Case (Var x) x uty [Alt DEFAULT [] u]

--   -- insert a seq for each var in the list of vars
--   let goBody :: [Var] -> CoreExpr -> Type -> CoreM CoreExpr
--       goBody xs body bodyTy = do

--         let go :: Var -> CoreExpr -> CoreM CoreExpr
--             go x acc = do
--               case splitTyConApp_maybe (varType x) of
--                 Just (ip, args) -> case instNewTyCon_maybe ip args of
--                   Just (argTy, coercion) -> do
--                     putMsg $ ppr $ force x acc bodyTy
--                     pure $! force x acc bodyTy
--                     -- putMsg $ ppr $ mkSeq argTy bodyTy (Cast (Var x) coercion) acc
--                   _ -> error "StrictImplParams: impossible"
--                 _ -> error "StrictImplParams: impossible"

--         foldrM go body xs

--   let goDef :: [Var] -> CoreExpr -> Type -> CoreM CoreExpr
--       goDef xs t a = case t of
--         Lam x t -> case a of
--           GHC.ForAllTy _ a  -> Lam x <$!> goDef xs t a
--           GHC.FunTy _ _ a b
--             | Just (getName -> con, _) <- splitTyConApp_maybe a, con == ipName ->
--                 Lam x <$!> goDef (setNoOccInfo x:xs) t b
--             | otherwise ->
--                 Lam x <$!> goDef xs t b
--           _ -> error "StrictImplParams: impossible"
--         t -> goBody xs t a

--   let goBind :: CoreBind -> CoreM CoreBind
--       goBind = \case
--         NonRec b t -> NonRec b <$!> goDef [] t (varType b)
--         Rec defs   -> Rec <$!> mapM (\(b, t) -> (b,) <$!> goDef [] t (varType b)) defs

--   bindsOnlyPass (mapM goBind) guts
