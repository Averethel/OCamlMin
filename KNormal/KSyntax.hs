module KNormal.KSyntax where
  import Utils.Iseq

  import Data.Set hiding (map)

  data FunDef = FD {
    name :: String,
    args :: [String],
    body :: KExpr
  } deriving Eq

  pprFunDef :: FunDef -> Iseq
  pprFunDef fd = iConcat [ iInterleave (iStr " ")
                            (map iStr $ name fd : args fd),
                          iStr " = ", pprKExpr (body fd) ]

  instance Show FunDef where
    show = show . pprFunDef

  data KExpr =
    -- constants
      KEunit
    | KEnil
    | KEint Integer
    -- unary ptimitives
    | KEneg String                              -- Bit negation
    | KEload String                             -- Dereference
    -- binary primitives
    | KEadd String String
    | KEsub String String
    | KEmult String String
    | KEdiv String String
    | KEmod String String
    | KEstore String String                     -- Assignment
    --
    | KEvar String
    | KEerror String
    | KEifEq String String KExpr KExpr
    | KEifLE String String KExpr KExpr
    | KElet String KExpr KExpr                  -- Functions are not allowed
    | KEletRec FunDef KExpr                     -- Annonymous functions will be named
    | KEapply String [String]
    | KEpair String String
    | KEcons String String
    | KEletPair String String String KExpr      -- Read from pair
    | KEletList String String String KExpr      -- Read from list
    | KEhandle KExpr KExpr
    | KEseq KExpr KExpr
    | KEextFunApp String [String]               -- External function application
    deriving Eq                                 --   Known external functions:
                                                --    - reference maker
                                                --    - tag getter

  freeVars :: KExpr -> Set String
  freeVars (KEneg s)              =
    singleton s
  freeVars (KEload s)             =
    singleton s
  freeVars (KEadd s1 s2)          =
    fromList [s1, s2]
  freeVars (KEsub s1 s2)          =
    fromList [s1, s2]
  freeVars (KEmult s1 s2)         =
    fromList [s1, s2]
  freeVars (KEdiv s1 s2)          =
    fromList [s1, s2]
  freeVars (KEmod s1 s2)          =
    fromList [s1, s2]
  freeVars (KEstore s1 s2)        =
    fromList [s1, s2]
  freeVars (KEvar s)              =
    singleton s
  freeVars (KEifEq s1 s2 e1 e2)   =
    s1 `insert` (s2 `insert` freeVars e1 `union` freeVars e2)
  freeVars (KEifLE s1 s2 e1 e2)   =
    s1 `insert` (s2 `insert` freeVars e1 `union` freeVars e2)
  freeVars (KElet s e1 e2)        =
    freeVars e1 `union` (s `delete` freeVars e2)
  freeVars (KEletRec fd e)        =
    freeVars (body fd) \\ fromList (args fd) `union`
    (name fd `delete` freeVars e)
  freeVars (KEapply s ss)         =
    fromList $ s:ss
  freeVars (KEpair s1 s2)         =
    fromList [s1, s2]
  freeVars (KEcons s1 s2)         =
    fromList [s1, s2]
  freeVars (KEletPair s1 s2 s3 e) =
    s3 `insert` freeVars e \\ fromList [s1, s2]
  freeVars (KEletList s1 s2 s3 e) =
    s3 `insert` freeVars e \\ fromList [s1, s2]
  freeVars (KEhandle e1 e2)       =
    freeVars e1 `union` freeVars e2
  freeVars (KEseq e1 e2)          =
    freeVars e1 `union` freeVars e2
  freeVars (KEextFunApp s ss)     =
    fromList $ s:ss
  freeVars _                      =
    empty

  pprKExpr :: KExpr -> Iseq
  pprKExpr KEunit                   = iStr "()"
  pprKExpr KEnil                    = iStr "[]"
  pprKExpr (KEint n)                = iStr . show $ n
  pprKExpr (KEneg s)                = iStr "-" `iAppend` iStr s
  pprKExpr (KEload s)               = iStr "&" `iAppend` iStr s
  pprKExpr (KEadd s1 s2)            = iConcat [ iStr s1, iStr " + ", iStr s2 ]
  pprKExpr (KEsub s1 s2)            = iConcat [ iStr s1, iStr " - ", iStr s2 ]
  pprKExpr (KEmult s1 s2)           = iConcat [ iStr s1, iStr " * ", iStr s2 ]
  pprKExpr (KEdiv s1 s2)            = iConcat [ iStr s1, iStr " / ", iStr s2 ]
  pprKExpr (KEmod s1 s2)            = iConcat [ iStr s1, iStr " % ", iStr s2 ]
  pprKExpr (KEstore s1 s2)          = iConcat [ iStr s1, iStr " := ", iStr s2 ]
  pprKExpr (KEvar s)                = iStr s
  pprKExpr (KEerror s)              = iStr s
  pprKExpr (KEifEq s1 s2 e1 e2)     = iConcat [ iStr "if ", iStr s1,
                                                iStr " == ", iStr s2, iStr "{",
                                                iNewline, indentation,
                                                iIndent $ pprKExpr e1,
                                                iStr " } else { ",
                                                iNewline, indentation,
                                                iIndent $ pprKExpr e2,
                                                iNewline, iStr "}" ]
  pprKExpr (KEifLE s1 s2 e1 e2)     = iConcat [ iStr "if ", iStr s1,
                                                iStr " <= ", iStr s2, iStr "{",
                                                iNewline, indentation,
                                                iIndent $ pprKExpr e1,
                                                iStr " } else { ",
                                                iNewline, indentation,
                                                iIndent $ pprKExpr e2,
                                                iNewline, iStr "}" ]
  pprKExpr (KElet s e1 e2)          = iConcat [ iStr "let ", iStr s,
                                                iStr " = ", pprKExpr e1,
                                                iNewline, iStr "in ",
                                                pprKExpr e2 ]
  pprKExpr (KEletRec fd e)          = iConcat [ iStr "letrec ", pprFunDef fd,
                                                iNewline, iStr "in ",
                                                pprKExpr e ]
  pprKExpr (KEapply s ss)           = iInterleave (iStr " ") $ map iStr (s:ss)
  pprKExpr (KEpair s1 s2)           = iConcat [ iStr "(", iStr s1, iStr ", ",
                                                iStr s2, iStr ")" ]
  pprKExpr (KEcons s1 s2)           = iConcat [ iStr s1, iStr "::", iStr s2 ]
  pprKExpr (KEletPair s1 s2 s3 e)   = iConcat [ iStr "let (", iStr s1,
                                                iStr ", ", iStr s2, iStr ") = ",
                                                iStr s3, iNewline, iStr "in ",
                                                pprKExpr e ]
  pprKExpr (KEletList s1 s2 s3 e)   = iConcat [ iStr "let ", iStr s1,
                                                iStr "::", iStr s2, iStr " = ",
                                                iStr s3, iNewline, iStr "in ",
                                                pprKExpr e ]
  pprKExpr (KEhandle e1 e2)         = iConcat [ pprKExpr e1, iNewline,
                                                iStr "rescue", iNewline,
                                                pprKExpr e2 ]
  pprKExpr (KEseq e1 e2)            = iConcat [ pprKExpr e1, iStr "; ",
                                                pprKExpr e2 ]
  pprKExpr (KEextFunApp s ss)       = iInterleave (iStr " ") $ map iStr (s:ss)

  instance Show KExpr where
    show = show . pprKExpr

