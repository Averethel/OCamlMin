module ClosureConvert.CSyntax where
  import Utils.Iseq

  data Label = L String deriving Eq

  instance Show Label where
    show (L s) = "_L_" ++ s

  data Closure = C {
    entry  :: Label,
    actFvs :: [String]
  } deriving Eq

  pprClosure :: Closure -> Iseq
  pprClosure c = iConcat [ iStr "{ ", iStr . show . entry $ c,
                           iStr ", [ ", iInterleave (iStr ", ") $
                           map iStr (actFvs c), iStr " }" ]

  instance Show Closure where
    show = show . pprClosure

  data CExpr =
    -- constants
      CEunit
    | CEnil
    | CEint Integer
    -- unary primitives
    | CEneg String
    | CEload String
    -- binary primitives
    | CEadd String String
    | CEsub String String
    | CEmult String String
    | CEdiv String String
    | CEmod String String
    | CEstore String String
    --
    | CEvar String
    | CEerror String
    | CEifEq String String CExpr CExpr
    | CEifLE String String CExpr CExpr
    | CElet String CExpr CExpr
    | CEmakeClj String Closure CExpr
    | CEappClj String [String]
    | CEappDir Label [String]
    | CEpair String String
    | CEcons String String
    | CEletPair String String String CExpr
    | CEletList String String String CExpr
    | CEhandle CExpr CExpr
    | CEseq CExpr CExpr
    deriving Eq

  pprCExpr :: CExpr -> Iseq
  pprCExpr CEunit                 = iStr "()"
  pprCExpr CEnil                  = iStr "[]"
  pprCExpr (CEint n)              = iStr . show $ n
  pprCExpr (CEneg s)              = iStr "-" `iAppend` iStr s
  pprCExpr (CEload s)             = iStr "&" `iAppend` iStr s
  pprCExpr (CEadd s1 s2)          = iConcat [ iStr s1, iStr " + ", iStr s2 ]
  pprCExpr (CEsub s1 s2)          = iConcat [ iStr s1, iStr " - ", iStr s2 ]
  pprCExpr (CEmult s1 s2)         = iConcat [ iStr s1, iStr " * ", iStr s2 ]
  pprCExpr (CEdiv s1 s2)          = iConcat [ iStr s1, iStr " / ", iStr s2 ]
  pprCExpr (CEmod s1 s2)          = iConcat [ iStr s1, iStr " % ", iStr s2 ]
  pprCExpr (CEstore s1 s2)        = iConcat [ iStr s1, iStr " := ", iStr s2 ]
  pprCExpr (CEvar s)              = iStr s
  pprCExpr (CEerror s)            = iStr s
  pprCExpr (CEifEq s1 s2 e1 e2)   = iConcat [ iStr "if ", iStr s1,
                                              iStr " == ", iStr s2, iStr "{",
                                              iNewline, indentation,
                                              iIndent $ pprCExpr e1,
                                              iStr " } else { ",
                                              iNewline, indentation,
                                              iIndent $ pprCExpr e2,
                                              iNewline, iStr "}" ]
  pprCExpr (CEifLE s1 s2 e1 e2)   = iConcat [ iStr "if ", iStr s1,
                                              iStr " <= ", iStr s2, iStr "{",
                                              iNewline, indentation,
                                              iIndent $ pprCExpr e1,
                                              iStr " } else { ",
                                              iNewline, indentation,
                                              iIndent $ pprCExpr e2,
                                              iNewline, iStr "}" ]
  pprCExpr (CElet s e1 e2)        = iConcat [ iStr "let ", iStr s,
                                              iStr " = ", pprCExpr e1,
                                              iNewline, iStr "in ",
                                              pprCExpr e2 ]
  pprCExpr (CEmakeClj s c e)      = iConcat [ iStr "make_closure( ", iStr s,
                                              pprClosure c, pprCExpr e,
                                              iStr " )" ]
  pprCExpr (CEappClj s ss)        = iConcat [ iStr s, iStr "@( ",
                                              iInterleave (iStr ", ") $
                                                map iStr ss,
                                              iStr " )" ]
  pprCExpr (CEappDir s ss)        = iInterleave (iStr " ") $ map iStr $
                                                                  show s:ss
  pprCExpr (CEpair s1 s2)         = iConcat [ iStr "(", iStr s1, iStr ", ",
                                              iStr s2, iStr ")" ]
  pprCExpr (CEcons s1 s2)         = iConcat [ iStr s1, iStr "::", iStr s2 ]
  pprCExpr (CEletPair s1 s2 s3 e) = iConcat [ iStr "let (", iStr s1,
                                              iStr ", ", iStr s2, iStr ") = ",
                                              iStr s3, iNewline, iStr "in ",
                                              pprCExpr e ]
  pprCExpr (CEletList s1 s2 s3 e) = iConcat [ iStr "let ", iStr s1,
                                              iStr "::", iStr s2, iStr " = ",
                                              iStr s3, iNewline, iStr "in ",
                                              pprCExpr e ]
  pprCExpr (CEhandle e1 e2)       = iConcat [ pprCExpr e1, iNewline,
                                              iStr "rescue", iNewline,
                                              pprCExpr e2 ]
  pprCExpr (CEseq e1 e2)          = iConcat [ pprCExpr e1, iStr "; ",
                                              pprCExpr e2 ]

  instance Show CExpr where
    show = show . pprCExpr

  data FunDef = FD {
    name      :: Label,
    args      :: [String],
    formalFvs :: [String],
    body      :: CExpr
  } deriving Eq

  pprFunDef :: FunDef -> Iseq
  pprFunDef fd = iConcat [ iStr . show . name $ fd, iStr " ",
                           iInterleave (iStr " ") $ map iStr $ args fd,
                           iStr " =", iNewline, indentation,
                           iIndent . pprCExpr . body $ fd,
                           iNewline, iStr "FVs = ",
                           iInterleave (iStr ", ") $ map iStr $ formalFvs fd ]

  instance Show FunDef where
    show = show . pprFunDef

  data Program = P {
    definitions :: [FunDef],
    main        :: CExpr
  } deriving Eq

  pprProgram :: Program -> Iseq
  pprProgram (P []  e) = pprCExpr e
  pprProgram (P fds e) = iConcat [ iInterleave (iStr ";;" `iAppend` iNewline) $
                                      map pprFunDef fds, iStr ";;", iNewline,
                                   pprCExpr e ]

  instance Show Program where
    show = show . pprProgram


