module Syntax.Expr where
  import Syntax.BinaryPrim
  import Syntax.Constant
  import Syntax.Pattern
  import Syntax.UnaryPrim

  import Utils.Errors
  import Utils.Iseq

  data FunClause = FC {
    arguments :: [Pattern],
    body      :: Expr
  } deriving Eq

  pprFunArgs :: [Pattern] -> Iseq
  pprFunArgs = iInterleave (iStr " ") . map pprAPattern

  pprFunClause :: FunClause -> Iseq
  pprFunClause fc = pprFunArgs (arguments fc) `iAppend` iStr " -> "
                                              `iAppend` pprExpr (body fc)

  pprFunClauses :: [FunClause] -> Iseq
  pprFunClauses = iInterleave (iConcat [ iNewline, iStr "| "]) .
                  map pprFunClause

  instance Show FunClause where
    show = show . pprFunClause

  data Expr =
      Econst  Constant
    | Euprim  UnaryPrim
    | Ebprim  BinaryPrim
    | Evar    String
    | Efun    [FunClause]
    | Elet    Pattern Expr Expr
    | Eletrec String [FunClause] Expr
    | Eapply  Expr [Expr]
    | Epair   Expr Expr
    | Econs   Expr Expr
    | Eif     Expr Expr Expr
    | Eseq    Expr Expr
    | Ehandle Expr Expr
    | EmatchFailure
    deriving Eq

  isAtomicExpr :: Expr -> Bool
  isAtomicExpr (Evar _)      = True
  isAtomicExpr (Econst _)    = True
  isAtomicExpr (Euprim _)    = True
  isAtomicExpr (Ebprim _)    = True
  isAtomicExpr _             = False

  pprAExpr :: Expr -> Iseq
  pprAExpr e
    | isAtomicExpr e = pprExpr e
    | otherwise      = iStr "(" `iAppend` pprExpr e `iAppend` iStr ")"

  pprArgs :: [Expr] -> Iseq
  pprArgs = iInterleave (iStr " ") . map pprAExpr

  pprApplication :: Expr -> [Expr] -> Iseq
  pprApplication (Ebprim p) [e1, e2]  = iConcat [ pprAExpr e1, iStr " ",
                                                  pprBinaryPrim p, iStr " ",
                                                  pprAExpr e2 ]
  pprApplication e          args      = iConcat [ pprExpr e, iStr " ",
                                                  pprArgs args ]

  pprExpr :: Expr -> Iseq
  pprExpr (Econst c)        = pprConstant c
  pprExpr (Euprim p)        = pprUnaryPrim p
  pprExpr (Ebprim p)        = pprBinaryPrim p
  pprExpr (Evar i)          = iStr i
  pprExpr (Efun fcs)        = iConcat [ iStr "function {", iNewline, indentation,
                                        iIndent $ iStr "  " `iAppend`
                                        pprFunClauses fcs, iNewline, iStr "}"]
  pprExpr (Elet p e1 e2)    = iConcat [ iStr "let ", pprAPattern p, iStr " = ",
                                        pprExpr e1, iStr " in ", pprExpr e2 ]
  pprExpr (Eletrec i fcs e) = iConcat [ iStr "letrec ", iStr i, iStr " = ",
                                        pprExpr (Efun fcs), iStr " in",
                                        iNewline, pprExpr e ]
  pprExpr (Eapply e args)   = pprApplication e args
  pprExpr (Epair e1 e2)     = iConcat [ iStr "(",  iInterleave (iStr ", ") $
                                        map pprAExpr [e1, e2], iStr ")" ]
  pprExpr (Econs e1 e2)     = pprAExpr e1 `iAppend` iStr " :: "
                                          `iAppend` pprAExpr e2
  pprExpr (Eif e1 e2 e3)    = iConcat [ iStr "if ( ", pprExpr e1,
                                        iStr ") then {", iNewline, indentation,
                                        iIndent $ pprExpr e2, iNewline,
                                        iStr "} else {", iNewline, indentation,
                                        iIndent $ pprExpr e3, iNewline,
                                        iStr "}" ]
  pprExpr (Eseq e1 e2)      = pprAExpr e1 `iAppend` iStr "; "
                                          `iAppend` pprAExpr e2
  pprExpr (Ehandle e1 e2)   = iConcat [ pprAExpr e1, iNewline, iStr "rescue",
                                        iNewline, pprAExpr e2 ]
  pprExpr EmatchFailure     = iStr matchFailure

  instance Show Expr where
    show = show . pprExpr
