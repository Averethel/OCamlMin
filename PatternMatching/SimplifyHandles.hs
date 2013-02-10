module PatternMatching.SimplifyHandles (simplifyHandles) where
  import Syntax.Expr

  cannotFailMatchingFunClause :: FunClause -> Bool
  cannotFailMatchingFunClause = cannotFailMatching . fbody

  cannotFailMatchingCaseClause :: CaseClause -> Bool
  cannotFailMatchingCaseClause = cannotFailMatching . cbody

  cannotFailMatching :: Expr -> Bool
  cannotFailMatching (Efun fcs)        =
    -- at this point we don't have to worry about incomlete matchings
    -- thosa have been taken care of in previos passes
    all cannotFailMatchingFunClause fcs
  cannotFailMatching (Elet _ e1 e2)    =
    cannotFailMatching e1 && cannotFailMatching e2
  cannotFailMatching (Eletrec _ fcs e) =
    all cannotFailMatchingFunClause fcs && cannotFailMatching e
  cannotFailMatching (Eapply e1 as)    =
    cannotFailMatching e1 && all cannotFailMatching as
  cannotFailMatching (Epair e1 e2)     =
    cannotFailMatching e1 && cannotFailMatching e2
  cannotFailMatching (Econs e1 e2)     =
    cannotFailMatching e1 && cannotFailMatching e2
  cannotFailMatching (Eif e1 e2 e3)    =
    cannotFailMatching e1 && cannotFailMatching e2 && cannotFailMatching e3
  cannotFailMatching (Eseq e1 e2)      =
    cannotFailMatching e1 && cannotFailMatching e2
  cannotFailMatching (Ecase e ccs)     =
    cannotFailMatching e && all cannotFailMatchingCaseClause ccs
  cannotFailMatching (Ehandle e1 e2)   =
    cannotFailMatching e1 && cannotFailMatching e2
  cannotFailMatching EmatchFailure     = False
  cannotFailMatching _                 = True

  simplifyHandles1FunClause :: FunClause -> FunClause
  simplifyHandles1FunClause fc =
    fc{ fbody = simplifyHandles1 $ fbody fc }

  simplifyHandles1CaseClause :: CaseClause -> CaseClause
  simplifyHandles1CaseClause cc =
    cc{ cbody = simplifyHandles1 $ cbody cc }

  simplifyHandles1 :: Expr -> Expr
  simplifyHandles1 (Efun fcs)                  =
    Efun $ map simplifyHandles1FunClause fcs
  simplifyHandles1 (Elet p e1 e2)              =
    Elet p (simplifyHandles1 e1) $ simplifyHandles1 e2
  simplifyHandles1 (Eletrec n fcs e)           =
    Eletrec n (map simplifyHandles1FunClause fcs) $ simplifyHandles1 e
  simplifyHandles1 (Eapply e1 as)              =
    Eapply (simplifyHandles1 e1) $ map simplifyHandles1 as
  simplifyHandles1 (Epair e1 e2)               =
    Epair (simplifyHandles1 e1) $ simplifyHandles1 e2
  simplifyHandles1 (Econs e1 e2)               =
    Econs (simplifyHandles1 e1) $ simplifyHandles1 e2
  simplifyHandles1 (Eif e1 e2 e3)              =
    Eif (simplifyHandles1 e1) (simplifyHandles1 e2) $
        simplifyHandles1 e3
  simplifyHandles1 (Eseq e1 e2)                =
    Eseq (simplifyHandles1 e1) $ simplifyHandles1 e2
  simplifyHandles1 (Ecase e1 ccs)              =
    Ecase (simplifyHandles1 e1) $ map simplifyHandles1CaseClause ccs
  simplifyHandles1 (Ehandle EmatchFailure e1)  =
    simplifyHandles1 e1
  simplifyHandles1 (Ehandle (Eif e1 e2 e3) e4)
    | cannotFailMatching (simplifyHandles1 e1) &&
      cannotFailMatching (simplifyHandles1 e2) =
      Eif (simplifyHandles1 e1) (simplifyHandles1 e2) $
          Ehandle (simplifyHandles1 e3) $ simplifyHandles1 e4
  simplifyHandles1 (Ehandle e1 EmatchFailure)  =
    simplifyHandles1 e1
  simplifyHandles1 (Ehandle e1 e2)
    | cannotFailMatching (simplifyHandles1 e1) =
      simplifyHandles1 e1
    | otherwise                               =
      Ehandle (simplifyHandles1 e1) $ simplifyHandles1 e2
  simplifyHandles1 e                           =
    e

  simplifyHandles :: Expr -> Expr
  simplifyHandles e
    | simplifyHandles1 e == e = e
    | otherwise               = simplifyHandles (simplifyHandles1 e)
