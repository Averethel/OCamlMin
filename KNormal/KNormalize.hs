{-# LANGUAGE
  FlexibleContexts
  #-}

module KNormal.KNormalize (kNormalize) where
  import KNormal.Counter
  import KNormal.KSyntax

  import Syntax

  import Control.Exception.Base
  import Control.Monad.State

  import Utils.Errors

  insertLet :: MonadState Counter m => KExpr -> (String -> m KExpr) -> m KExpr
  insertLet (KEvar x) k = k x
  insertLet e         k = do
    x  <- freshVar
    e' <- k x
    return $ KElet x e e'

  kNormalizeConstant :: Constant -> KExpr
  kNormalizeConstant (Cint n)   = KEint n
  kNormalizeConstant (Cbool b)  = KEint $ if b then 1 else 0
  kNormalizeConstant Cnil       = KEnil
  kNormalizeConstant Cunit      = KEunit

  mkFunDef :: MonadState Counter m => String -> FunClause -> m FunDef
  mkFunDef n fc = do
    let as = map (\(Pvar x) -> x) $ arguments fc
    b <- kNormalize $ fbody fc
    return FD{ name = n, body = b, args = as }

  kNormalizeUPrim :: MonadState Counter m => UnaryPrim -> Expr -> m KExpr
  kNormalizeUPrim UPnot   e =
    kNormalize $ Eif e (Econst $ Cbool False) $ Econst $ Cbool True
  kNormalizeUPrim UPref   e = do
    e' <- kNormalize e
    insertLet e' (\x -> return $ KEextFunApp "create_ref" [x])
  kNormalizeUPrim UPderef e = do
    e' <- kNormalize e
    insertLet e' (return . KEload)
  kNormalizeUPrim UPminus e = do
    e' <- kNormalize e
    insertLet e' (return . KEneg)

  kNormalizeOp :: MonadState Counter m =>
                  (String -> String -> KExpr) -> Expr -> Expr -> m KExpr
  kNormalizeOp op e1 e2 = do
    e1' <- kNormalize e1
    insertLet e1' (\x -> do {
      e2' <- kNormalize e2;
      insertLet e2' (return . op x)})

  kNormalizeBPrim :: MonadState Counter m =>
                     BinaryPrim -> Expr -> Expr -> m KExpr
  kNormalizeBPrim BPeq     e1 e2  = do
    e1' <- kNormalize e1
    insertLet e1' (\x -> do {
      e2' <- kNormalize e2;
      insertLet e2' (\y -> return $ KEifEq x y (KEint 1) (KEint 0))})
  kNormalizeBPrim BPlt     e1 e2  = do
    e1' <- kNormalize e1
    insertLet e1' (\x -> do {
      e2' <- kNormalize e2;
      insertLet e2' (\y -> return $ KEifLE x y
        (KEifEq x y (KEint 0) (KEint 1)) (KEint 0))})
  kNormalizeBPrim BPgt     e1 e2  = do
    e1' <- kNormalize e1
    insertLet e1' (\x -> do {
      e2' <- kNormalize e2;
      insertLet e2' (\y -> return $ KEifLE x y (KEint 0) (KEint 1))})
  kNormalizeBPrim BPor     e1 e2  = do
    e1' <- kNormalize e1
    insertLet e1' (\x -> do {
      e2' <- kNormalize e2;
      v   <- freshVar;
      return $ KElet v (KEint 1) $ KEifEq x v (KEint 1) e2' })
  kNormalizeBPrim BPand    e1 e2  = do
    e1' <- kNormalize e1
    insertLet e1' (\x -> do {
      e2' <- kNormalize e2;
      v   <- freshVar;
      return $ KElet v (KEint 1) $ KEifEq x v e2' (KEint 0) })
  kNormalizeBPrim BPadd    e1 e2  =
    kNormalizeOp KEadd e1 e2
  kNormalizeBPrim BPsub    e1 e2  =
    kNormalizeOp KEsub e1 e2
  kNormalizeBPrim BPmult   e1 e2  =
    kNormalizeOp KEmult e1 e2
  kNormalizeBPrim BPdiv    e1 e2  =
    kNormalizeOp KEdiv e1 e2
  kNormalizeBPrim BPmod    e1 e2  =
    kNormalizeOp KEmod e1 e2
  kNormalizeBPrim BPassign e1 e2  =
    kNormalizeOp KEstore e1 e2

  kNormalizeArgs :: MonadState Counter m =>
                    [Expr] -> m ([String], KExpr -> KExpr)
  kNormalizeArgs []     = return ([], id)
  kNormalizeArgs (a:as) = do
    (as', f) <- kNormalizeArgs as
    a'       <- kNormalize a
    case a' of
      KEvar x -> return (x:as', f)
      _       -> do
        v <- freshVar
        return (v:as', KElet v a' . f)


  kNormalizeCaseBool :: MonadState Counter m =>
                        String -> Expr -> Expr -> m KExpr
  kNormalizeCaseBool n et ef = do
    v   <- freshVar
    et' <- kNormalize et
    ef' <- kNormalize ef
    return $ KElet v (KEint 1) $ KEifEq n v et' ef'

  genVars :: MonadState Counter m => String -> m (KExpr, String, String)
  genVars n = do
    let e' = KEextFunApp "tag_of" [n]
    v1  <- freshVar
    v2  <- freshVar
    return (e', v1, v2)

  kNormalizeCaseList :: MonadState Counter m =>
                        String -> Expr -> String -> String -> Expr -> m KExpr
  kNormalizeCaseList n en x xs ec = do
    (e', v1, v2) <- genVars n
    v3  <- freshVar
    en' <- kNormalize en
    ec' <- kNormalize ec
    return $ KElet v1 e' $ KElet v2 (KEint 0) $ KElet v2 (KEint 1) $
             KEifEq v1 v2 en' $ KEifEq v1 v3 (KEletList x xs n ec') $
             KEerror matchFailure



  kNormalizeCase :: MonadState Counter m =>
                    [CaseClause] -> String -> m KExpr
  -- pair
  kNormalizeCase [CC { constructor = CNpair,
                       variables   = [a, b],
                       cbody       = cb }]    n = do
    (e', v1, v2) <- genVars n
    cb' <- kNormalize cb
    return $ KElet v1 (KEint 0) $
             KElet v2 e' $ KEifEq v1 v2 (KEletPair a b n cb')
              (KEerror matchFailure)
  -- boolean
  kNormalizeCase [CC { constructor = CNtrue,
                       variables   = [],
                       cbody       = bt },
                  CC { constructor = CNfalse,
                       variables   = [],
                       cbody       = bf }]    n =
    kNormalizeCaseBool n bt bf
  kNormalizeCase [CC { constructor = CNfalse,
                       variables   = [],
                       cbody       = bf },
                  CC { constructor = CNtrue,
                       variables   = [],
                       cbody       = bt }]    n =
    kNormalizeCaseBool n bt bf
  -- list
  kNormalizeCase [CC { constructor = CNnil,
                       variables   = [],
                       cbody       = bn },
                  CC { constructor = CNcons,
                       variables   = [x, xs],
                       cbody       = bc }]    n =
    kNormalizeCaseList n bn x xs bc
  kNormalizeCase [CC { constructor = CNcons,
                       variables   = [x, xs],
                       cbody       = bc },
                  CC { constructor = CNnil,
                       variables   = [],
                       cbody       = bn }]    n =
    kNormalizeCaseList n bn x xs bc
  -- unit
  kNormalizeCase [CC { constructor = CNunit,
                       variables   = [],
                       cbody       = b }]     n = do
    (e', v1, v2) <- genVars n
    b' <- kNormalize b
    return $ KElet v1 (KEint 0) $
             KElet v2 e' $ KEifEq v1 v2 b'
              (KEerror matchFailure)
  kNormalizeCase ccs n = assert False $ kNormalizeCase ccs n


  kNormalize :: MonadState Counter m => Expr -> m KExpr
  kNormalize (Econst c)                               =
    return $ kNormalizeConstant c
  kNormalize (Evar s)                                 =
    -- Here should be checking for external references
    -- when modules are implemented
    return $ KEvar s
  kNormalize (Elet (Pvar s)            (Efun fcs) e2)     = do
    fd  <- mkFunDef s $ head fcs
    e2' <- kNormalize e2
    return $ KEletRec fd e2'
  kNormalize (Elet (Pvar s)                    e1 e2)     = do
    e1' <- kNormalize e1
    e2' <- kNormalize e2
    return $ KElet s e1' e2'
  kNormalize (Elet (Ppair (Pvar p1) (Pvar p2)) e1 e2)     = do
    e1' <- kNormalize e1
    insertLet e1' (\x -> do {
      e2' <- kNormalize e2;
      return $ KEletPair p1 p2 x e2' })
  kNormalize (Elet (Pcons (Pvar p1) (Pvar p2)) e1 e2)     = do
    e1' <- kNormalize e1
    insertLet e1' (\x -> do {
      e2' <- kNormalize e2;
      return $ KEletList p1 p2 x e2' })
  kNormalize (Eletrec s fcs e)                            = do
    fd <- mkFunDef s $ head fcs
    e' <- kNormalize e
    return $ KEletRec fd e'
  kNormalize (Eapply (Euprim up) [e])                     =
    kNormalizeUPrim up e
  kNormalize (Eapply (Ebprim bp) [e1, e2])                =
    kNormalizeBPrim bp e1 e2
  kNormalize (Eapply (Efun fcs)  as)                      = do
    l         <- freshLambda
    fd        <- mkFunDef l $ head fcs
    (as', lt) <- kNormalizeArgs as
    return $ KEletRec fd $ lt $ KEapply l as'
  kNormalize (Eapply (Evar x)    as)                      = do
    (as', lt) <- kNormalizeArgs as
    return $ lt $ KEapply x as'
  kNormalize (Epair e1 e2)                                =
    kNormalizeOp KEpair e1 e2
  kNormalize (Econs e1 e2)                                =
    kNormalizeOp KEcons e1 e2
  kNormalize (Eif (Eapply (Euprim UPnot) [c1]) e2 e3)     =
    kNormalize (Eif c1 e3 e2)
  kNormalize (Eif (Eapply (Ebprim BPeq) [c1, c2]) e2 e3)  = do
    c1' <- kNormalize c1
    insertLet c1' (\x -> do {
      c2' <- kNormalize c2;
      insertLet c2' (\y -> do {
        e2' <- kNormalize e2;
        e3' <- kNormalize e3;
        return $ KEifEq x y e2' e3'})})
  kNormalize (Eif (Eapply (Ebprim BPgt) [c1, c2]) e2 e3)  = do
    c1' <- kNormalize c1
    insertLet c1' (\x -> do {
      c2' <- kNormalize c2;
      insertLet c2' (\y -> do {
        e2' <- kNormalize e2;
        e3' <- kNormalize e3;
        return $ KEifLE x y e3' e2'})})
  kNormalize (Eif e1 e2 e3)                               = do
    e1' <- kNormalize e1
    insertLet e1' (\x -> do {
      y   <- freshVar;
      e2' <- kNormalize e2;
      e3' <- kNormalize e3;
      return $ KElet y (KEint 1) $ KEifEq x y e2' e3' })
  kNormalize (Eseq e1 e2)                                 = do
    e1' <- kNormalize e1
    e2' <- kNormalize e2
    return $ KEseq e1' e2'
  kNormalize (Ecase e ccs)                                = do
    e' <- kNormalize e
    insertLet e' (kNormalizeCase ccs)
  kNormalize (Ehandle e1 e2)                              = do
    e1' <- kNormalize e1
    e2' <- kNormalize e2
    return $ KEhandle e1' e2'
  kNormalize EmatchFailure                                =
    return $ KEerror matchFailure
  kNormalize e                  =
    assert False (kNormalize e)
