module TypedSyntax.Expr where
  import TypedSyntax.BinaryPrim
  import TypedSyntax.Constant
  import TypedSyntax.Constructor
  import TypedSyntax.Pattern
  import TypedSyntax.UnaryPrim
  import Types

  import Utils.Errors
  import Utils.Iseq

  data TypedFunClause = TFC {
    tfcArguments :: [TypedPattern],
    tfcBody      :: TypedExpr
  } deriving Eq

  typeOfTypedFunClause :: TypedFunClause -> Type
  typeOfTypedFunClause tfc = Tfun (map typeOfTypedPattern $ tfcArguments tfc) $
                                  typeOfTypedExpr $ tfcBody tfc

  applySubstTFC :: TypedFunClause -> Subst -> TypedFunClause
  applySubstTFC tfc s = tfc {
    tfcArguments = map (`applySubstTP` s) $ tfcArguments tfc,
    tfcBody      = tfcBody tfc `applySubstTE` s
  }

  pprTypedFunArgs :: [TypedPattern] -> Iseq
  pprTypedFunArgs = iInterleave (iStr " ") . map pprTypedAPattern

  pprTypedFunClause :: TypedFunClause -> Iseq
  pprTypedFunClause fc = pprTypedFunArgs (tfcArguments fc) `iAppend` iStr " -> "
                        `iAppend` pprTypedExpr (tfcBody fc)

  pprTypedFunClauses :: [TypedFunClause] -> Iseq
  pprTypedFunClauses = iInterleave (iConcat [ iNewline, iStr "| "]) .
                        map pprTypedFunClause

  instance Show TypedFunClause where
    show = show . pprTypedFunClause

  data TypedCaseClause = TCC {
    tccConstructor :: TypedConstructor,
    tccVariables   :: [(String, Type)],
    tccBody        :: TypedExpr
  } deriving Eq

  typeOfTypedCaseClause :: TypedCaseClause -> Type
  typeOfTypedCaseClause tcc = Tfun [tcon] $ typeOfTypedExpr $ tccBody tcc where
    tcon = case snd . tccConstructor $ tcc of
      Tfun [_] t -> t
      t          -> t

  applySubstTCC :: TypedCaseClause -> Subst -> TypedCaseClause
  applySubstTCC tcc s = tcc {
    tccConstructor = (fst . tccConstructor $ tcc,
                      (snd . tccConstructor $ tcc) `applySubst` s),
    tccVariables   = map (\(v, t) -> (v, t `applySubst` s)) $ tccVariables tcc,
    tccBody        = tccBody tcc `applySubstTE` s
  }

  pprTypedCaseClause :: TypedCaseClause -> Iseq
  pprTypedCaseClause cc = pprTypedConstructor (tccConstructor cc) `iAppend`
                          iStr " " `iAppend` iInterleave (iStr " ")
                          (map (\(v, t) -> iConcat [iStr v, iStr " : ",
                          pprType t]) $ tccVariables cc) `iAppend`
                          iStr " -> " `iAppend` pprTypedExpr (tccBody cc)

  pprTypedCaseClauses :: [TypedCaseClause] -> Iseq
  pprTypedCaseClauses = iInterleave (iConcat [ iNewline, iStr "| "]) .
                   map pprTypedCaseClause

  instance Show TypedCaseClause where
    show = show . pprTypedCaseClause

  data TypedExpr =
      TEconst   TypedConstant
    | TEuprim   TypedUnaryPrim
    | TEbprim   TypedBinaryPrim
    | TEvar     String Type
    | TEfun     [TypedFunClause] Type
    | TElet     TypedPattern TypedExpr TypedExpr Type
    | TEletrec  String Type [TypedFunClause] TypedExpr Type
    | TEapply   TypedExpr [TypedExpr] Type
    | TEpair    TypedExpr TypedExpr Type
    | TEcons    TypedExpr TypedExpr Type
    | TEif      TypedExpr TypedExpr TypedExpr Type
    | TEseq     TypedExpr TypedExpr Type
    | TEcase    TypedExpr [TypedCaseClause] Type
    | TEhandle  TypedExpr TypedExpr Type
    | TEmatchFailure Type
    deriving Eq

  typeOfTypedExpr :: TypedExpr -> Type
  typeOfTypedExpr (TEconst (_, t))      = t
  typeOfTypedExpr (TEuprim (_, t))      = t
  typeOfTypedExpr (TEbprim (_, t))      = t
  typeOfTypedExpr (TEvar _ t)           = t
  typeOfTypedExpr (TEfun _ t)           = t
  typeOfTypedExpr (TElet _ _ _ t)       = t
  typeOfTypedExpr (TEletrec _ _ _ _ t)  = t
  typeOfTypedExpr (TEapply _ _ t)       = t
  typeOfTypedExpr (TEpair _ _ t)        = t
  typeOfTypedExpr (TEcons _ _ t)        = t
  typeOfTypedExpr (TEif _ _ _ t)        = t
  typeOfTypedExpr (TEseq _ _ t)         = t
  typeOfTypedExpr (TEcase _ _ t)        = t
  typeOfTypedExpr (TEhandle _ _ t)      = t
  typeOfTypedExpr (TEmatchFailure t)    = t

  applySubstTE :: TypedExpr -> Subst -> TypedExpr
  applySubstTE (TEconst (c, t))       s = TEconst (c, t `applySubst` s)
  applySubstTE (TEuprim (u, t))       s = TEuprim (u, t `applySubst` s)
  applySubstTE (TEbprim (b, t))       s = TEbprim (b, t `applySubst` s)
  applySubstTE (TEvar v t)            s = TEvar v $ t `applySubst` s
  applySubstTE (TEfun f t)            s = TEfun (map (`applySubstTFC` s) f) $
                                            t `applySubst` s
  applySubstTE (TElet p e1 e2 t)      s = TElet (p `applySubstTP` s)
                                                (e1 `applySubstTE` s)
                                                (e2 `applySubstTE` s) $
                                                t `applySubst` s
  applySubstTE (TEletrec v t1 f e t2) s = TEletrec v (t1 `applySubst` s)
                                                   (map (`applySubstTFC` s) f)
                                                   (e `applySubstTE` s) $
                                                   t2 `applySubst` s
  applySubstTE (TEapply e es t)       s = TEapply (e `applySubstTE` s)
                                                  (map (`applySubstTE` s) es) $
                                                  t `applySubst` s
  applySubstTE (TEpair e1 e2 t)       s = TEpair (e1 `applySubstTE` s)
                                                 (e2 `applySubstTE` s) $
                                                 t `applySubst` s
  applySubstTE (TEcons e1 e2 t)       s = TEcons (e1 `applySubstTE` s)
                                                 (e2 `applySubstTE` s) $
                                                 t `applySubst` s
  applySubstTE (TEif e1 e2 e3 t)      s = TEif (e1 `applySubstTE` s)
                                               (e2 `applySubstTE` s)
                                               (e3 `applySubstTE` s) $
                                               t `applySubst` s
  applySubstTE (TEseq e1 e2 t)        s = TEseq (e1 `applySubstTE` s)
                                                (e2 `applySubstTE` s) $
                                                t `applySubst` s
  applySubstTE (TEcase e1 cc t)       s = TEcase (e1 `applySubstTE` s)
                                                 (map (`applySubstTCC` s) cc) $
                                                 t `applySubst` s
  applySubstTE (TEhandle e1 e2 t)     s = TEhandle (e1 `applySubstTE` s)
                                                   (e2 `applySubstTE` s) $
                                                   t `applySubst` s
  applySubstTE (TEmatchFailure t)     s = TEmatchFailure $ t `applySubst` s

  isAtomicTypedExpr :: TypedExpr -> Bool
  isAtomicTypedExpr (TEvar _ _)   = True
  isAtomicTypedExpr (TEconst _)   = True
  isAtomicTypedExpr (TEuprim _)   = True
  isAtomicTypedExpr (TEbprim _)   = True
  isAtomicTypedExpr _             = False

  pprTypedAExpr :: TypedExpr -> Iseq
  pprTypedAExpr e
    | isAtomicTypedExpr e = pprTypedExpr e
    | otherwise           = iStr "(" `iAppend` pprTypedExpr e `iAppend` iStr ")"

  pprTypedArgs :: [TypedExpr] -> Iseq
  pprTypedArgs = iInterleave (iStr " ") . map pprTypedAExpr

  pprTypedApplication :: TypedExpr -> [TypedExpr] -> Type -> Iseq
  pprTypedApplication (TEbprim p) [e1, e2]  t = iConcat [ pprTypedAExpr e1,
                                                          iStr " ",
                                                          pprTypedBinaryPrim p,
                                                          iStr " ",
                                                          pprTypedAExpr e2,
                                                          iStr " : ", pprType t]
  pprTypedApplication e          args       t = iConcat [ pprTypedExpr e,
                                                          iStr " ",
                                                          pprTypedArgs args,
                                                          iStr " : ", pprType t]

  pprTypedExpr :: TypedExpr -> Iseq
  pprTypedExpr (TEconst c)            = pprTypedConstant c
  pprTypedExpr (TEuprim p)            = pprTypedUnaryPrim p
  pprTypedExpr (TEbprim p)            = pprTypedBinaryPrim p
  pprTypedExpr (TEvar i t)            = iConcat [ iStr i, iStr " : ",
                                                  pprType t ]
  pprTypedExpr (TEfun fcs t)          = iConcat [ iStr "function {", iNewline,
                                                  indentation, iIndent $
                                                  iStr "  " `iAppend`
                                                  pprTypedFunClauses fcs,
                                                  iNewline, iStr "} : ",
                                                  pprType t ]
  pprTypedExpr (TElet p e1 e2 t)      = iConcat [ iStr "let ",
                                                  pprTypedAPattern p,
                                                  iStr " = ", pprTypedExpr e1,
                                                  iStr " in ", pprTypedExpr e2,
                                                  iStr " : ", pprType t]
  pprTypedExpr (TEletrec i d fcs e t) = iConcat [ iStr "letrec ", iStr i,
                                                  iStr " : ", pprType d,
                                                  iStr " = ", pprTypedExpr
                                                  (TEfun fcs d), iStr " in",
                                                  iNewline, pprTypedExpr e,
                                                  iStr " : ", pprType t ]
  pprTypedExpr (TEapply e args t)     = pprTypedApplication e args t
  pprTypedExpr (TEpair e1 e2 t)       = iConcat [ iStr "(",  iInterleave
                                                (iStr ", ") $ map pprTypedAExpr
                                                [e1, e2], iStr ") : ",
                                                pprType t]
  pprTypedExpr (TEcons e1 e2 t)       = iConcat [ pprTypedAExpr e1, iStr " :: ",
                                                  pprTypedAExpr e2, iStr " : ",
                                                  pprType t ]
  pprTypedExpr (TEif e1 e2 e3 t)      = iConcat [ iStr "if ( ", pprTypedExpr e1,
                                                  iStr ") then {", iNewline,
                                                  indentation, iIndent $
                                                  pprTypedExpr e2, iNewline,
                                                  iStr "} else {", iNewline,
                                                  indentation, iIndent $
                                                  pprTypedExpr e3, iNewline,
                                                  iStr "} : ", pprType t ]
  pprTypedExpr (TEseq e1 e2 t)        = iConcat [ pprTypedAExpr e1, iStr "; ",
                                                  pprTypedAExpr e2, iStr " : ",
                                                  pprType t ]
  pprTypedExpr (TEcase arg cls t)     = iConcat [ iStr "case ",
                                                  pprTypedAExpr arg,
                                                  iStr " of {", iNewline,
                                                  indentation, iIndent $
                                                  iStr "  " `iAppend`
                                                  pprTypedCaseClauses cls,
                                                  iNewline, iStr "} : ",
                                                  pprType t ]
  pprTypedExpr (TEhandle e1 e2 t)     = iConcat [ pprTypedAExpr e1, iNewline,
                                                  iStr "rescue", iNewline,
                                                  pprTypedAExpr e2, iStr " : ",
                                                  pprType t ]
  pprTypedExpr (TEmatchFailure t)     = iConcat [ iStr matchFailure, iStr " : ",
                                                  pprType t ]

  instance Show TypedExpr where
    show = show . pprTypedExpr

