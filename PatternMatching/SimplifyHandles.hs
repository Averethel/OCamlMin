module PatternMatching.SimplifyHandles (simplifyHandles) where
  import TypedSyntax.Expr

  cannotFailMatchingFunClause :: TypedFunClause -> Bool
  cannotFailMatchingFunClause = cannotFailMatching . tfcBody

  cannotFailMatchingCaseClause :: TypedCaseClause -> Bool
  cannotFailMatchingCaseClause = cannotFailMatching . tccBody

  cannotFailMatching :: TypedExpr -> Bool
  cannotFailMatching (TEfun fcs _)          =
    -- at this point we don't have to worry about incomlete matchings
    -- thosa have been taken care of in previos passes
    all cannotFailMatchingFunClause fcs
  cannotFailMatching (TElet _ e1 e2 _)      =
    cannotFailMatching e1 && cannotFailMatching e2
  cannotFailMatching (TEletrec _ _ fcs e _) =
    all cannotFailMatchingFunClause fcs && cannotFailMatching e
  cannotFailMatching (TEapply e1 as _)      =
    cannotFailMatching e1 && all cannotFailMatching as
  cannotFailMatching (TEpair e1 e2 _)       =
    cannotFailMatching e1 && cannotFailMatching e2
  cannotFailMatching (TEcons e1 e2 _)       =
    cannotFailMatching e1 && cannotFailMatching e2
  cannotFailMatching (TEif e1 e2 e3 _)      =
    cannotFailMatching e1 && cannotFailMatching e2 && cannotFailMatching e3
  cannotFailMatching (TEseq e1 e2 _)        =
    cannotFailMatching e1 && cannotFailMatching e2
  cannotFailMatching (TEcase e ccs _)       =
    cannotFailMatching e && all cannotFailMatchingCaseClause ccs
  cannotFailMatching (TEhandle e1 e2 _)     =
    cannotFailMatching e1 && cannotFailMatching e2
  cannotFailMatching (TEmatchFailure _)     = False
  cannotFailMatching _                      = True

  simplifyHandles1FunClause :: TypedFunClause -> TypedFunClause
  simplifyHandles1FunClause fc =
    fc{ tfcBody = simplifyHandles1 $ tfcBody fc }

  simplifyHandles1CaseClause :: TypedCaseClause -> TypedCaseClause
  simplifyHandles1CaseClause cc =
    cc{ tccBody = simplifyHandles1 $ tccBody cc }

  simplifyHandles1 :: TypedExpr -> TypedExpr
  simplifyHandles1 (TEfun fcs t)                      =
    TEfun (map simplifyHandles1FunClause fcs) t
  simplifyHandles1 (TElet p e1 e2 t)                  =
    TElet p (simplifyHandles1 e1) (simplifyHandles1 e2) t
  simplifyHandles1 (TEletrec n t1 fcs e t2)           =
    TEletrec n t1 (map simplifyHandles1FunClause fcs) (simplifyHandles1 e) t2
  simplifyHandles1 (TEapply e1 as t)                  =
    TEapply (simplifyHandles1 e1) (map simplifyHandles1 as) t
  simplifyHandles1 (TEpair e1 e2 t)                   =
    TEpair (simplifyHandles1 e1) (simplifyHandles1 e2) t
  simplifyHandles1 (TEcons e1 e2 t)                   =
    TEcons (simplifyHandles1 e1) (simplifyHandles1 e2) t
  simplifyHandles1 (TEif e1 e2 e3 t)                  =
    TEif (simplifyHandles1 e1) (simplifyHandles1 e2) (simplifyHandles1 e3) t
  simplifyHandles1 (TEseq e1 e2 t)                    =
    TEseq (simplifyHandles1 e1) (simplifyHandles1 e2) t
  simplifyHandles1 (TEcase e1 ccs t)                  =
    TEcase (simplifyHandles1 e1) (map simplifyHandles1CaseClause ccs) t
  simplifyHandles1 (TEhandle (TEmatchFailure _) e1 _) =
    simplifyHandles1 e1
  simplifyHandles1 (TEhandle (TEif e1 e2 e3 i) e4 t)
    | cannotFailMatching (simplifyHandles1 e1) &&
      cannotFailMatching (simplifyHandles1 e2)        =
      TEif (simplifyHandles1 e1) (simplifyHandles1 e2)
           (TEhandle (simplifyHandles1 e3) (simplifyHandles1 e4) t) i
  simplifyHandles1 (TEhandle e1 (TEmatchFailure _) _) =
    simplifyHandles1 e1
  simplifyHandles1 (TEhandle e1 e2 t)
    | cannotFailMatching (simplifyHandles1 e1)        =
      simplifyHandles1 e1
    | otherwise                                       =
      TEhandle (simplifyHandles1 e1) (simplifyHandles1 e2) t
  simplifyHandles1 e                                  =
    e

  simplifyHandles :: TypedExpr -> TypedExpr
  simplifyHandles e
    | simplifyHandles1 e == e = e
    | otherwise               = simplifyHandles (simplifyHandles1 e)
