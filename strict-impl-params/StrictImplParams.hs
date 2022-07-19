
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
  installCoreToDos = \_ todo -> pure (CoreDoPluginPass "Strict Implicit Params" pass : todo),
  pluginRecompile  = purePlugin
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

forceType :: Type -> Type
forceType a = case tcView a of
  Just a' -> forceType a'
  _       -> a

pass :: ModGuts -> CoreM ModGuts
pass guts = do
  dflags <- getDynFlags
  ipName <- fromTHName ''GHC.Classes.IP

  let goDef :: [Var] -> CoreExpr -> Type -> CoreExpr
      goDef xs t a = case t of
        Lam x t -> case forceType a of
          GHC.ForAllTy _ a  -> Lam x $! goDef xs t a
          GHC.FunTy _ _ a b
            | Just (getName -> con, _) <- splitTyConApp_maybe a, con == ipName ->
                Lam x $! goDef (((:) $! setNoOccInfo x) xs) t b
            | otherwise ->
                Lam x $! goDef xs t b
          a -> do
            error $ "unexpected lam type: " ++ showSDoc dflags (ppr a)

        t -> foldl' (\acc x -> forceVar x acc a) t xs

  let goBind :: CoreBind -> CoreBind
      goBind = \case
        NonRec b t -> NonRec b $! goDef [] t (varType b)
        Rec defs   -> Rec $! map' (\(b, t) -> (b,) $! goDef [] t (varType b)) defs

  pure $! guts {mg_binds = map' goBind (mg_binds guts)}
