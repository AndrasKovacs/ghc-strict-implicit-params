
{-# language LambdaCase, Strict, TupleSections, CPP, MagicHash #-}
{-# options_ghc -Wincomplete-patterns -Wunused-imports #-}

module StrictImplParams (plugin) where

import Data.Maybe
import GHC.Core.Class (classMethods)
import GHC.Core.Predicate
import GHC.Plugins

import qualified GHC.Core.TyCo.Rep as GHC
import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)

#if __GLASGOW_HASKELL__ >= 914
import GHC.Types.Demand
#endif

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = \_ todo -> pure (CoreDoPluginPass "Strict Implicit Params" pass : todo
#if __GLASGOW_HASKELL__ >= 914
                                       ++ [CoreDoPluginPass "Strict Implicit Params (call sites)" callPass]
#endif
                                      ),
  pluginRecompile  = purePlugin
  }

-- | Pointer equality. We use it to return the original node when none of its children changed,
--   so that unchanged Core is not reallocated. False negatives only cause reallocation.
same :: a -> a -> Bool
same x y = isTrue# (reallyUnsafePtrEquality# x y)
{-# inline same #-}

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
  (tc, args)   <- splitTyConApp_maybe (varType x)
  cls          <- tyConClass_maybe tc
  (sym, valTy) <- case args of [sym, valTy] -> Just (sym, valTy); _ -> Nothing
  sel          <- case classMethods cls of [sel] -> Just sel; _ -> Nothing
  let val = mkCoreApps (Var sel) [Type sym, Type valTy, Var x]
  pure (val, valTy)

-- | Force the value of an implicit param, continue with CoreExpr body that has Type
--   type.
--
--   Note: we force the *value* stored in the dictionary, by applying the class method
--   to it.
forceVar :: Var -> CoreExpr -> Type -> CoreExpr
forceVar x body bodyTy = case ipValue x of
  Just (val, valTy) -> let wild = mkWildValBinder manyType valTy in
                       Case val wild bodyTy [Alt DEFAULT [] body]
  Nothing           -> error "forceVar: not an implicit parameter dictionary"

-- | Force the implicit params in the list, continue with CoreExpr body that has Type type.
forceVars :: [Var] -> CoreExpr -> Type -> CoreExpr
forceVars vars body bodyTy = case vars of
  []       -> body
  x : vars -> forceVars vars (forceVar x body bodyTy) bodyTy

pass :: ModGuts -> CoreM ModGuts
pass guts = do
  let binds' = goBinds (mg_binds guts)
  pure $! guts {mg_binds = binds'}

goBinds :: [CoreBind] -> [CoreBind]
goBinds bbs = case bbs of
  []   -> bbs
  b:bs -> let b' = goBind b; bs' = goBinds bs in
          if same b b' && same bs bs' then bbs else b':bs'

goBind :: CoreBind -> CoreBind
goBind b = case b of
  NonRec x t -> let t' = goDef [] t (varType x) in
                if same t t' then b else NonRec x t'
  Rec defs   -> let defs' = goDefs defs in
                if same defs defs' then b else Rec defs'

goDefs :: [(Id, CoreExpr)] -> [(Id, CoreExpr)]
goDefs ddefs = case ddefs of
  []              -> ddefs
  def@(x, t):defs -> let t' = goDef [] t (varType x); defs' = goDefs defs in
                     if same t t' then
                       if same defs defs' then ddefs else def:defs'
                     else (x, t'):defs'

-- | Traverse a definition with the given type, forcing the implicit params bound by the
--   leading lambdas. The list contains the implicit param binders bound so far.
goDef :: [Var] -> CoreExpr -> Type -> CoreExpr
goDef vars e a = case e of
  Lam x t -> case coreFullView a of
    -- Note: a Core type lambda binds its own type variable, which is not necessarily the
    -- one bound by the ForAllTy in the definition's type. Since we use the def type to
    -- generate the Core of the forcing, we need to rename the type binder to match the
    -- lambda binder. We sincerely hope that Core names are unique and this renaming doesn't
    -- introduce shadowing.
    GHC.ForAllTy bndr a -> let tv = binderVar bndr
                               a' | isTyVar x, tv /= x = substTyWith [tv] [mkTyVarTy x] a
                                  | otherwise          = a
                               t' = goDef vars t a'
                           in if same t t' then e else Lam x t'
    -- Precaution: we add an occurrence of the binder, so whatever occurrence info it
    -- carries from the desugarer should be outdated.
    GHC.FunTy _ _ a b | isImplicitParamTy a -> let x' = zapIdOccInfo x; t' = goDef (x':vars) t b in
                                               Lam x' t'
                      | otherwise           -> let t' = goDef vars t b in
                                               if same t t' then e else Lam x t'
    _ -> pprPanic "unexpected type for lambda expression" (ppr a)
  _ ->
    forceVars vars (goExpr e) a

goExpr :: CoreExpr -> CoreExpr
goExpr e = case e of
  Var{}                  -> e
  Lit{}                  -> e
  Type{}                 -> e
  Coercion{}             -> e
  App t u                -> let t' = goExpr t; u' = goExpr u in
                            if same t t' && same u u' then e else App t' u'
  Case t scr bodyty alts -> let t' = goExpr t; alts' = goAlts alts in
                            if same t t' && same alts alts' then e else Case t' scr bodyty alts'
  Cast t coe             -> let t' = goExpr t in
                            if same t t' then e else Cast t' coe
  Tick tck t             -> let t' = goExpr t in
                            if same t t' then e else Tick tck t'

  -- Note: the bound var of a free-standing lambda
  -- does not get forced!
  Lam x t                -> let t' = goExpr t in
                            if same t t' then e else Lam x t'

  -- Neither does an implicit let binder!
  -- In both cases the issue is that we don't know the type
  -- of the expr body and I don't like the idea of recomputing it!
  Let b t                -> let b' = goBind b; t' = goExpr t in
                            if same b b' && same t t' then e else Let b' t'

goAlts :: [CoreAlt] -> [CoreAlt]
goAlts aalts = case aalts of
  []                        -> aalts
  alt@(Alt con bs rhs):alts -> let rhs' = goExpr rhs; alts' = goAlts alts in
                               if same rhs rhs' then
                                 if same alts alts' then aalts else alt:alts'
                               else Alt con bs rhs':alts'

#if __GLASGOW_HASKELL__ >= 914
-- | Starting from GHC 9.14, implicit parameter dictionaries are represented in Core by a
--   constructor application @C:IP \@x \@a e@ (which is erased in STG), instead of a newtype
--   cast. Since a constructor application is a value, CorePrep does not evaluate @e@ at
--   strict call sites, but allocates a thunk for it, even when the callee's demand signature
--   says that it strictly uses the field (which is the case for all functions strictified
--   by 'pass').
--
--   This pass runs at the end of the Core pipeline, and rewrites calls
--   @f .. (C:IP \@x \@a e) ..@ to @case e of v { DEFAULT -> f .. (C:IP \@x \@a v) .. }@
--   whenever @f@'s demand signature is strict in the dictionary field.
--   We only do this for non-trivial @e@.
callPass :: ModGuts -> CoreM ModGuts
callPass guts = do
  tag <- getUniqTag
  liftIO $ do binds <- cpBinds tag (mg_binds guts)
              pure $! guts {mg_binds = binds}

cpBinds :: Char -> [CoreBind] -> IO [CoreBind]
cpBinds tag bbs = case bbs of
  []   -> pure bbs
  b:bs -> do b' <- cpBind tag b; bs' <- cpBinds tag bs
             if same b b' && same bs bs' then pure bbs else pure (b':bs')

cpBind :: Char -> CoreBind -> IO CoreBind
cpBind tag b = case b of
  NonRec x t -> do t' <- cpExpr tag t
                   if same t t' then pure b else pure (NonRec x t')
  Rec defs   -> do defs' <- cpDefs tag defs
                   if same defs defs' then pure b else pure (Rec defs')

cpDefs :: Char -> [(Id, CoreExpr)] -> IO [(Id, CoreExpr)]
cpDefs tag ddefs = case ddefs of
  []                 -> pure ddefs
  def@(x, t):defs -> do t' <- cpExpr tag t; defs' <- cpDefs tag defs
                        if same t t' then
                          if same defs defs' then pure ddefs else pure (def:defs')
                        else pure ((x, t'):defs')

cpAlts :: Char -> [CoreAlt] -> IO [CoreAlt]
cpAlts tag aalts = case aalts of
  []                      -> pure aalts
  alt@(Alt con bs rhs):alts -> do rhs' <- cpExpr tag rhs; alts' <- cpAlts tag alts
                                  if same rhs rhs' then
                                    if same alts alts' then pure aalts else pure (alt:alts')
                                  else pure (Alt con bs rhs':alts')

cpExpr :: Char -> CoreExpr -> IO CoreExpr
cpExpr tag e = case e of
  Var{}            -> pure e
  Lit{}            -> pure e
  Type{}           -> pure e
  Coercion{}       -> pure e
  App{}            -> case appHead e of
    -- Demands only apply to saturated calls. The rightmost value arg has index n - 1.
    Var f | dmds <- fst (splitDmdSig (idDmdSig f)), not (null dmds)
          , n <- spineValArgs e 0, n >= length dmds
          , needsEval dmds (n - 1) e
          -> cpCallEval tag dmds (n - 1) e
    _     -> cpSpine tag e
  Lam x t          -> do t' <- cpExpr tag t
                         if same t t' then pure e else pure (Lam x t')
  Let b t          -> do b' <- cpBind tag b; t' <- cpExpr tag t
                         if same b b' && same t t' then pure e else pure (Let b' t')
  Case t x ty alts -> do t' <- cpExpr tag t; alts' <- cpAlts tag alts
                         if same t t' && same alts alts' then pure e else pure (Case t' x ty alts')
  Cast t co        -> do t' <- cpExpr tag t
                         if same t t' then pure e else pure (Cast t' co)
  Tick tck t       -> do t' <- cpExpr tag t
                         if same t t' then pure e else pure (Tick tck t')

-- | Rebuild an application spine without rewriting it. We must not call 'cpExpr' on the
--   function part of an 'App', since that would treat a partial application as a call.
cpSpine :: Char -> CoreExpr -> IO CoreExpr
cpSpine tag e = case e of
  App f a -> do f' <- cpSpine tag f; a' <- cpExpr tag a
                if same f f' && same a a' then pure e else pure (App f' a')
  _       -> cpExpr tag e

-- | Rebuild a call spine where at least one argument must be evaluated before the call.
cpCallEval :: Char -> [Demand] -> Int -> CoreExpr -> IO CoreExpr
cpCallEval tag dmds i t = do
  (call, evals) <- cpSpineEval tag dmds i t []
  let ty = exprType call
  seqType ty `seq` (pure $! wrapEvals ty evals call)

-- | Rebuild a call spine, returning the (scrutinee, binder) pairs to evaluate before the call.
--   @i@ is the index of the rightmost value arg in the spine.
cpSpineEval :: Char -> [Demand] -> Int -> CoreExpr -> [(CoreExpr, Id)]
            -> IO (CoreExpr, [(CoreExpr, Id)])
cpSpineEval tag dmds i t evals = case t of
  App f a
    | isTypeArg a -> do
        (f', evals') <- cpSpineEval tag dmds i f evals
        pure (App f' a, evals')
    | App con e <- a, isEvalArg dmds i a -> do
        e' <- cpExpr tag e
        u  <- uniqFromTag tag
        let ty = ipDictValTy con
            v  = mkSysLocal (fsLit "ipv") u manyType ty
        (f', evals') <- cpSpineEval tag dmds (i - 1) f ((e', v):evals)
        pure (App f' (App con (Var v)), evals')
    | otherwise -> do
        a' <- cpExpr tag a
        (f', evals') <- cpSpineEval tag dmds (i - 1) f evals
        pure (App f' a', evals')
  _ -> do t' <- cpExpr tag t; pure (t', evals)

wrapEvals :: Type -> [(CoreExpr, Id)] -> CoreExpr -> CoreExpr
wrapEvals ty evals body = case evals of
  []           -> body
  (e, v):evals -> wrapEvals ty evals (Case e v ty [Alt DEFAULT [] body])

appHead :: CoreExpr -> CoreExpr
appHead = \case
  App f _ -> appHead f
  t       -> t

spineValArgs :: CoreExpr -> Int -> Int
spineValArgs t acc = case t of
  App f a | isTypeArg a -> spineValArgs f acc
          | otherwise   -> spineValArgs f (acc + 1)
  _                     -> acc

-- | Does any value arg of the spine have to be evaluated before the call? @i@ is the index of
--   the rightmost value arg.
needsEval :: [Demand] -> Int -> CoreExpr -> Bool
needsEval dmds i = \case
  App f a | isTypeArg a         -> needsEval dmds i f
          | isEvalArg dmds i a  -> True
          | otherwise           -> needsEval dmds (i - 1) f
  _                             -> False

-- | Is the arg @C:IP \@x \@a e@ with non-trivial and unevaluated @e@, which is strictly used
--   by the @i@-th demand?
isEvalArg :: [Demand] -> Int -> CoreArg -> Bool
isEvalArg dmds i = \case
  App con e | isIPDictCon con, not (exprIsTrivial e), not (exprIsHNF e) -> case drop i dmds of
    d:_ -> strictField d
    []  -> False
  _ -> False

-- | Is the (single) field of a dictionary strictly used by the demand?
--   Note: we don't use 'viewProd' because it allocates field demands for 'Poly'.
strictField :: Demand -> Bool
strictField (_ :* sd) = case sd of
  Prod _ [fd] -> isStrUsedDmd fd
  Poly _ n    -> isStrict n && not (isAbs n) -- every field has cardinality n
  _           -> False

-- | Match @C:IP \@x \@a@.
isIPDictCon :: CoreExpr -> Bool
isIPDictCon = \case
  App (App (Var dc) (Type _)) (Type _) -> isDataConWorkId dc && isIPTyCon (dataConTyCon (idDataCon dc))
  _                                    -> False

-- | The value type @a@ of @C:IP \@x \@a@.
ipDictValTy :: CoreExpr -> Type
ipDictValTy = \case
  App _ (Type a) -> a
  _              -> error "ipDictValTy: not an implicit parameter dictionary constructor"
#endif
