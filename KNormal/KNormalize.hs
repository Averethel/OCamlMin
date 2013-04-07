{-# LANGUAGE
  FlexibleContexts
  #-}

module KNormal.KNormalize (kNormalize) where
  import KNormal.Counter
  import KNormal.KSyntax

  import Syntax
  import TypedSyntax
  import Types

  import Control.Exception.Base
  import Control.Monad.State

  import Utils.Errors

  insertLet :: MonadState Counter m => KExpr -> (String -> Type -> m KExpr) ->
               m KExpr
  insertLet (KEvar x t) k = k x t
  insertLet e           k = do
    let t = typeOfKExpr e
    x  <- freshVar t
    e' <- k x t
    return $ KElet (x, t) e e' $ typeOfKExpr e'

  kNormalizeConstant :: TypedConstant -> KExpr
  kNormalizeConstant (Cint n, t)   = KEint n t
  kNormalizeConstant (Cbool b, _)  = KEint (if b then 1 else 0) Tint
  kNormalizeConstant (Cnil, t)     = KEnil t
  kNormalizeConstant (Cunit, t)    = KEunit t

  mkFunDef :: MonadState Counter m => String -> TypedFunClause -> m FunDef
  mkFunDef n fc = do
    let as = map (\(TPvar x t) -> (x, t)) $ tfcArguments fc
    b <- kNormalize $ tfcBody fc
    return FD{ name = (n, typeOfTypedFunClause fc), body = b, args = as }

  kNormalizeUPrim :: MonadState Counter m => TypedUnaryPrim -> TypedExpr ->
                     m KExpr
  kNormalizeUPrim (UPnot, _)   e =
    kNormalize $ TEif e (TEconst (Cbool False, Tbool))
                        (TEconst (Cbool True, Tbool)) Tbool
  kNormalizeUPrim (UPref, t)   e = do
    e' <- kNormalize e
    insertLet e' (\x t' ->
      return $ KEextFunApp ("create_ref", t) [(x, t')] $ Tref t')
  kNormalizeUPrim (UPderef, t) e = do
    let Tfun [_] t1 = t
    e' <- kNormalize e
    insertLet e' (\x t' -> return $ KEload (x, t') t1)
  kNormalizeUPrim (UPminus, t) e = do
    let Tfun [_] t1 = t
    e' <- kNormalize e
    insertLet e' (\x t' -> return $ KEneg (x, t') t1)

  kNormalizeOp :: MonadState Counter m =>
                  ((String, Type) -> (String, Type) -> Type -> KExpr) ->
                  Type -> TypedExpr -> TypedExpr -> m KExpr
  kNormalizeOp op t e1 e2 = do
    let Tfun [_, _] t' = t
    e1' <- kNormalize e1
    insertLet e1' (\x1 t1 -> do {
      e2' <- kNormalize e2;
      insertLet e2' (\x2 t2 -> return $ op (x1, t1) (x2, t2) t' )})

  kNormalizeBPrim :: MonadState Counter m =>
                     TypedBinaryPrim -> TypedExpr -> TypedExpr -> m KExpr
  kNormalizeBPrim (BPeq, _)     e1 e2  = do
    e1' <- kNormalize e1
    insertLet e1' (\x1 t1 -> do {
      e2' <- kNormalize e2;
      insertLet e2' (\x2 t2 ->
        return $ KEifEq (x1, t1) (x2, t2) (KEint 1 Tint) (KEint 0 Tint) Tint )})
  kNormalizeBPrim (BPlt, _)     e1 e2  = do
    e1' <- kNormalize e1
    insertLet e1' (\x1 t1 -> do {
      e2' <- kNormalize e2;
      insertLet e2' (\x2 t2 ->
        return $ KEifLE (x1, t1) (x2, t2)
          (KEifEq (x1, t1) (x2, t2) (KEint 0 Tint) (KEint 1 Tint) Tint)
          (KEint 0 Tint) Tint)})
  kNormalizeBPrim (BPgt, _)     e1 e2  = do
    e1' <- kNormalize e1
    insertLet e1' (\x1 t1 -> do {
      e2' <- kNormalize e2;
      insertLet e2' (\x2 t2 ->
        return $ KEifLE (x1, t1) (x2, t2) (KEint 0 Tint) (KEint 1 Tint) Tint)})
  kNormalizeBPrim (BPor, _)     e1 e2  = do
    e1' <- kNormalize e1
    insertLet e1' (\x t-> do {
      e2' <- kNormalize e2;
      v   <- freshVar Tint;
      return $ KElet (v, Tint) (KEint 1 Tint)
               (KEifEq (x, t) (v, Tint)  (KEint 1 Tint) e2' Tint) Tint })
  kNormalizeBPrim (BPand, _)    e1 e2  = do
    e1' <- kNormalize e1
    insertLet e1' (\x t -> do {
      e2' <- kNormalize e2;
      v   <- freshVar Tint;
      return $ KElet (v, Tint) (KEint 1 Tint)
                (KEifEq (x, t) (v, Tint) e2' (KEint 0 Tint) Tint) Tint })
  kNormalizeBPrim (BPadd, t)    e1 e2  =
    kNormalizeOp KEadd t e1 e2
  kNormalizeBPrim (BPsub, t)    e1 e2  =
    kNormalizeOp KEsub t e1 e2
  kNormalizeBPrim (BPmult, t)   e1 e2  =
    kNormalizeOp KEmult t e1 e2
  kNormalizeBPrim (BPdiv, t)    e1 e2  =
    kNormalizeOp KEdiv t e1 e2
  kNormalizeBPrim (BPmod, t)    e1 e2  =
    kNormalizeOp KEmod t e1 e2
  kNormalizeBPrim (BPassign, t) e1 e2  =
    kNormalizeOp KEstore t e1 e2

  kNormalizeArgs :: MonadState Counter m =>
                    [TypedExpr] -> m ([(String, Type)], KExpr -> KExpr)
  kNormalizeArgs []     = return ([], id)
  kNormalizeArgs (a:as) = do
    (as', f) <- kNormalizeArgs as
    a'       <- kNormalize a
    case a' of
      KEvar x t -> return ((x, t):as', f)
      _       -> do
        let t = typeOfKExpr a'
        v <- freshVar t
        return ((v, t):as', \e -> let e' = (f e) in
          KElet (v, t) a' e' $ typeOfKExpr e')


  kNormalizeCaseBool :: MonadState Counter m =>
                        String -> TypedExpr -> TypedExpr -> m KExpr
  kNormalizeCaseBool n et ef = do
    v   <- freshVar Tbool
    et' <- kNormalize et
    ef' <- kNormalize ef
    let t' = typeOfKExpr et'
    return $ KElet (v, Tint) (KEint 1 Tint)
              (KEifEq (n, Tint) (v, Tint) et' ef' t') t'

  genVars :: MonadState Counter m => String -> Type ->
             m (KExpr, (String, Type), (String, Type))
  genVars n t = do
    let e' = KEextFunApp ("tag_of", Tfun [t] Tint) [(n, t)] Tint
    v1  <- freshVar Tint
    v2  <- freshVar Tint
    return (e', (v1, Tint), (v2, Tint))

  kNormalizeCaseList :: MonadState Counter m => String -> Type -> TypedExpr ->
                        String -> Type -> String -> Type -> TypedExpr ->
                        m KExpr
  kNormalizeCaseList n t en x tx xs txs ec = do
    (e', (v1, t1), v2) <- genVars n t
    v3  <- freshVar t1
    en' <- kNormalize en
    ec' <- kNormalize ec
    let tp = typeOfKExpr ec'
    return $
      KElet (v1, t1) e' (
        KElet v2 (KEint 0 Tint) (
          KEifEq (v1, t1) v2 en' (
            KElet (v3, t1) (KEint 1 Tint) (
              KEifEq (v1, t1) (v3, t1)
                (KEletList (x, tx) (xs, txs) (n, t) ec' tp)
                (KEerror matchFailure tp) tp
              ) tp ) tp ) tp ) tp



  kNormalizeCase :: MonadState Counter m =>
                    [TypedCaseClause] -> String -> m KExpr
  -- pair
  kNormalizeCase [TCC { tccConstructor = (CNpair, t),
                        tccVariables   = [a, b],
                        tccBody        = cb }]    n = do
    (e', v1, v2) <- genVars n t
    cb' <- kNormalize cb
    let tp = typeOfKExpr cb'
    return $ KElet v1 (KEint 0 Tint)
             (KElet v2 e' (KEifEq v1 v2 (KEletPair a b (n, t) cb' tp)
                                        (KEerror matchFailure tp) tp) tp) tp
  -- boolean
  kNormalizeCase [TCC { tccConstructor = (CNtrue, Tbool),
                       tccVariables   = [],
                       tccBody        = bt },
                  TCC { tccConstructor = (CNfalse, Tbool),
                       tccVariables   = [],
                       tccBody        = bf }]     n =
    kNormalizeCaseBool n bt bf
  kNormalizeCase [TCC { tccConstructor = (CNfalse, Tbool),
                       tccVariables   = [],
                       tccBody        = bf },
                  TCC { tccConstructor = (CNtrue, Tbool),
                       tccVariables   = [],
                       tccBody        = bt }]    n =
    kNormalizeCaseBool n bt bf
  -- list
  kNormalizeCase [TCC { tccConstructor = (CNnil, _),
                       tccVariables   = [],
                       tccBody        = bn },
                  TCC { tccConstructor = (CNcons, _),
                       tccVariables   = [(x, tx), (xs, txs)],
                       tccBody        = bc }]    n =
    kNormalizeCaseList n txs bn x tx xs txs bc
  kNormalizeCase [TCC { tccConstructor = (CNcons, _),
                       tccVariables   = [(x, tx), (xs, txs)],
                       tccBody        = bc },
                  TCC { tccConstructor = (CNnil, _),
                       tccVariables   = [],
                       tccBody        = bn }]    n =
    kNormalizeCaseList n txs bn x tx xs txs bc
  ---- unit
  kNormalizeCase [TCC { tccConstructor = (CNunit, Tunit),
                       tccVariables   = [],
                       tccBody        = b }]     n = do
    (e', v1, v2) <- genVars n Tunit
    b' <- kNormalize b
    let tp = typeOfKExpr b'
    return $ KElet v1 (KEint 0 Tint)
              (KElet v2 e'
                (KEifEq v1 v2 b'
                (KEerror matchFailure tp) tp) tp) tp
  kNormalizeCase ccs n = assert False $ kNormalizeCase ccs n


  kNormalize :: MonadState Counter m => TypedExpr -> m KExpr
  kNormalize (TEconst c)                                                =
    return $ kNormalizeConstant c
  kNormalize (TEvar s t)                                                =
    -- Here should be checking for external references
    -- when modules are implemented
    return $ KEvar s t
  kNormalize (TEfun fcs tp)                                             = do
    l  <- freshLambda tp
    fd <- mkFunDef l $ head fcs
    return $ KEletRec fd (KEvar l tp) tp
  kNormalize (TElet (TPvar s _) (TEfun fcs _) e2 tp)                    = do
    fd  <- mkFunDef s $ head fcs
    e2' <- kNormalize e2
    return $ KEletRec fd e2' tp
  kNormalize (TElet (TPvar s t) e1 e2 tp)                               = do
    e1' <- kNormalize e1
    e2' <- kNormalize e2
    return $ KElet (s, t) e1' e2' tp
  kNormalize (TElet (TPpair (TPvar p1 t1) (TPvar p2 t2) _) e1 e2 tp)    = do
    e1' <- kNormalize e1
    insertLet e1' (\x t'-> do {
      e2' <- kNormalize e2;
      return $ KEletPair (p1, t1) (p2, t2) (x, t') e2' tp })
  kNormalize (TElet (TPcons (TPvar p1 t1) (TPvar p2 t2) _) e1 e2 tp)    = do
    e1' <- kNormalize e1
    insertLet e1' (\x t' -> do {
      e2' <- kNormalize e2;
      return $ KEletList (p1, t1) (p2, t2) (x, t') e2' tp })
  kNormalize (TEletrec s _ fcs e tp)                                    = do
    fd <- mkFunDef s $ head fcs
    e' <- kNormalize e
    return $ KEletRec fd e' tp
  kNormalize (TEapply (TEuprim up) [e] _)                               =
    kNormalizeUPrim up e
  kNormalize (TEapply (TEbprim bp) [e1, e2] _)                          =
    kNormalizeBPrim bp e1 e2
  kNormalize (TEapply (TEfun fcs t) as tp)                              = do
    l         <- freshLambda t
    fd        <- mkFunDef l $ head fcs
    (as', lt) <- kNormalizeArgs as
    return $ KEletRec fd (lt (KEapply (l, t) as' tp)) tp
  kNormalize (TEapply (TEvar x t)    as tp)                             = do
    (as', lt) <- kNormalizeArgs as
    return $ lt $ KEapply (x, t) as' tp
  kNormalize (TEpair e1 e2 tp)                                          =
    kNormalizeOp KEpair (Tfun [typeOfTypedExpr e1, typeOfTypedExpr e2] tp) e1 e2
  kNormalize (TEcons e1 e2 tp)                                          =
    kNormalizeOp KEcons (Tfun [typeOfTypedExpr e1, typeOfTypedExpr e2] tp) e1 e2
  kNormalize (TEif (TEapply (TEuprim (UPnot, _)) [c1] _) e2 e3 tp)      =
    kNormalize (TEif c1 e3 e2 tp)
  kNormalize (TEif (TEapply (TEbprim (BPeq, _)) [c1, c2] _) e2 e3 tp)   = do
    c1' <- kNormalize c1
    insertLet c1' (\x tx -> do {
      c2' <- kNormalize c2;
      insertLet c2' (\y ty-> do {
        e2' <- kNormalize e2;
        e3' <- kNormalize e3;
        return $ KEifEq (x, tx) (y, ty) e2' e3' tp})})
  kNormalize (TEif (TEapply (TEbprim (BPgt, _)) [c1, c2] _) e2 e3 tp)   = do
    c1' <- kNormalize c1
    insertLet c1' (\x tx -> do {
      c2' <- kNormalize c2;
      insertLet c2' (\y ty -> do {
        e2' <- kNormalize e2;
        e3' <- kNormalize e3;
        return $ KEifLE (x, tx) (y, ty) e3' e2' tp})})
  kNormalize (TEif e1 e2 e3 tp)                                         = do
    e1' <- kNormalize e1
    insertLet e1' (\x t -> do {
      y   <- freshVar Tint;
      e2' <- kNormalize e2;
      e3' <- kNormalize e3;
      return $ KElet (y, Tint) (KEint 1 Tint)
                (KEifEq (x, t) (y, Tint) e2' e3' tp) tp })
  kNormalize (TEseq e1 e2 tp)                                           = do
    e1' <- kNormalize e1
    e2' <- kNormalize e2
    return $ KEseq e1' e2' tp
  kNormalize (TEcase e ccs _)                                           = do
    e' <- kNormalize e
    insertLet e' (\x _ -> kNormalizeCase ccs x)
  kNormalize (TEhandle e1 e2 tp)                                        = do
    e1' <- kNormalize e1
    e2' <- kNormalize e2
    return $ KEhandle e1' e2' tp
  kNormalize (TEmatchFailure tp)                                        =
    return $ KEerror matchFailure tp
  kNormalize e                  =
    assert False (kNormalize e)
