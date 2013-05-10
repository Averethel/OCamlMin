module KNormal.KSyntax where
  import Types
  import Utils.Iseq

  import Data.Set hiding (map)

  data FunDef = FD {
    name :: (String, Type),
    args :: [(String, Type)],
    body :: KExpr
  } deriving Eq

  pprFunDef :: FunDef -> Iseq
  pprFunDef fd = iConcat [ iInterleave (iStr " ") $ (iStr . fst . name $ fd) :
                           map (\(v, t) -> iConcat [ iStr v, iStr " : ",
                           pprType t ]) (args fd), iStr " = ", pprKExpr $
                           body fd, iStr " : ", pprType . snd . name $ fd ]

  instance Show FunDef where
    show = show . pprFunDef

  data KExpr =
    -- constants
      KEunit Type
    | KEnil Type
    | KEint Integer Type
    -- unary ptimitives
    | KEneg (String, Type) Type                                                 -- Bit negation
    | KEload (String, Type) Type                                                -- Dereference
    -- binary primitives
    | KEadd (String, Type) (String, Type) Type
    | KEsub (String, Type) (String, Type) Type
    | KEmult (String, Type) (String, Type) Type
    | KEdiv (String, Type) (String, Type) Type
    | KEstore (String, Type) (String, Type) Type                                -- Assignment
    --
    | KEvar String Type
    | KEerror String Type
    | KEifEq (String, Type) (String, Type) KExpr KExpr Type
    | KEifLE (String, Type) (String, Type) KExpr KExpr Type
    | KElet (String, Type) KExpr KExpr Type                                     -- Functions are not allowed
    | KEletRec FunDef KExpr Type                                                -- Annonymous functions will be named
    | KEapply (String, Type) [(String, Type)] Type
    | KEpair (String, Type) (String, Type) Type
    | KEcons (String, Type) (String, Type) Type
    | KEletPair (String, Type) (String, Type) (String, Type) KExpr Type         -- Read from pair
    | KEletList (String, Type) (String, Type) (String, Type) KExpr Type         -- Read from list
    | KEhandle KExpr KExpr Type
    | KEseq KExpr KExpr Type
    | KEextFunApp (String, Type) [(String, Type)] Type                          -- External function application
    deriving Eq                                                                 --   Known external functions:
                                                                                --    - reference maker
                                                                                --    - tag getter

  freeVars :: KExpr -> Set String
  freeVars (KEneg (s, _) _)                         =
    singleton s
  freeVars (KEload (s, _) _)                        =
    singleton s
  freeVars (KEadd (s1, _) (s2, _) _)                =
    fromList [s1, s2]
  freeVars (KEsub (s1, _) (s2, _) _)                =
    fromList [s1, s2]
  freeVars (KEmult (s1, _) (s2, _) _)               =
    fromList [s1, s2]
  freeVars (KEdiv (s1, _) (s2, _) _)                =
    fromList [s1, s2]
  freeVars (KEstore (s1, _) (s2, _) _)              =
    fromList [s1, s2]
  freeVars (KEvar s _)                              =
    singleton s
  freeVars (KEifEq (s1, _) (s2, _) e1 e2 _)         =
    s1 `insert` (s2 `insert` freeVars e1 `union` freeVars e2)
  freeVars (KEifLE (s1, _) (s2, _) e1 e2 _)         =
    s1 `insert` (s2 `insert` freeVars e1 `union` freeVars e2)
  freeVars (KElet (s, _) e1 e2 _)                   =
    freeVars e1 `union` (s `delete` freeVars e2)
  freeVars (KEletRec fd e _)                        =
    freeVars (body fd) \\ fromList (map fst $ args fd) `union`
    (fst (name fd) `delete` freeVars e)
  freeVars (KEapply s ss _)                         =
    fromList . map fst $ s:ss
  freeVars (KEpair (s1, _) (s2, _) _)               =
    fromList [s1, s2]
  freeVars (KEcons (s1, _) (s2, _) _)               =
    fromList [s1, s2]
  freeVars (KEletPair (s1, _) (s2, _) (s3, _) e _)  =
    s3 `insert` freeVars e \\ fromList [s1, s2]
  freeVars (KEletList (s1, _) (s2, _) (s3, _) e _)  =
    s3 `insert` freeVars e \\ fromList [s1, s2]
  freeVars (KEhandle e1 e2 _)                       =
    freeVars e1 `union` freeVars e2
  freeVars (KEseq e1 e2 _)                          =
    freeVars e1 `union` freeVars e2
  freeVars (KEextFunApp s ss _)                     =
    fromList . map fst $ s:ss
  freeVars _                                        =
    empty

  typeOfKExpr :: KExpr -> Type
  typeOfKExpr (KEunit t)            = t
  typeOfKExpr (KEnil t)             = t
  typeOfKExpr (KEint _ t)           = t
  typeOfKExpr (KEneg _ t)           = t
  typeOfKExpr (KEload _ t)          = t
  typeOfKExpr (KEadd _ _ t)         = t
  typeOfKExpr (KEsub _ _ t)         = t
  typeOfKExpr (KEmult _ _ t)        = t
  typeOfKExpr (KEdiv _ _ t)         = t
  typeOfKExpr (KEstore _ _ t)       = t
  typeOfKExpr (KEvar _ t)           = t
  typeOfKExpr (KEerror _ t)         = t
  typeOfKExpr (KEifEq _ _ _ _ t)    = t
  typeOfKExpr (KEifLE _ _ _ _ t)    = t
  typeOfKExpr (KElet _ _ _ t)       = t
  typeOfKExpr (KEletRec _ _ t)      = t
  typeOfKExpr (KEapply _ _ t)       = t
  typeOfKExpr (KEpair _ _ t)        = t
  typeOfKExpr (KEcons _ _ t)        = t
  typeOfKExpr (KEletPair _ _ _ _ t) = t
  typeOfKExpr (KEletList _ _ _ _ t) = t
  typeOfKExpr (KEhandle _ _ t)      = t
  typeOfKExpr (KEseq _ _ t)         = t
  typeOfKExpr (KEextFunApp _ _ t)   = t

  pprKExpr :: KExpr -> Iseq
  pprKExpr (KEunit t)                                 =
    iConcat [ iStr "() : ", pprType t ]
  pprKExpr (KEnil t)                                  =
    iConcat [ iStr "[] : ", pprType t ]
  pprKExpr (KEint n t)                                =
    iConcat [ iStr . show $ n, iStr " : ", pprType t ]
  pprKExpr (KEneg (s, t1) t2)                         =
    iConcat [ iStr "-(", iStr s, iStr " : ", pprType t1, iStr ") : ",
              pprType t2 ]
  pprKExpr (KEload (s, t1) t2)                        =
    iConcat [ iStr "&(", iStr s, iStr " : ", pprType t1, iStr ") : ",
              pprType t2 ]
  pprKExpr (KEadd (s1, t1) (s2, t2) t3)               =
    iConcat [ iStr "(", iStr s1, iStr " : ", pprType t1, iStr ") + (", iStr s2,
              iStr " : ", pprType t2, iStr ") : ", pprType t3 ]
  pprKExpr (KEsub (s1, t1) (s2, t2) t3)               =
    iConcat [ iStr "(", iStr s1, iStr " : ", pprType t1, iStr ") - (", iStr s2,
              iStr " : ", pprType t2, iStr ") : ", pprType t3 ]
  pprKExpr (KEmult (s1, t1) (s2, t2) t3)              =
    iConcat [ iStr "(", iStr s1, iStr " : ", pprType t1, iStr ") * (", iStr s2,
              iStr " : ", pprType t2, iStr ") : ", pprType t3 ]
  pprKExpr (KEdiv (s1, t1) (s2, t2) t3)               =
    iConcat [ iStr "(", iStr s1, iStr " : ", pprType t1, iStr ") / (", iStr s2,
              iStr " : ", pprType t2, iStr ") : ", pprType t3 ]
  pprKExpr (KEstore (s1, t1) (s2, t2) t3)             =
    iConcat [ iStr "(", iStr s1, iStr " : ", pprType t1, iStr ") := (", iStr s2,
              iStr " : ", pprType t2, iStr ") : ", pprType t3 ]
  pprKExpr (KEvar s t)                                =
    iConcat [ iStr s, iStr " : ", pprType t ]
  pprKExpr (KEerror s t)                              =
    iConcat [ iStr s, iStr " : ", pprType t ]
  pprKExpr (KEifEq (s1, t1) (s2, t2) e1 e2 t)         =
    iConcat [ iStr "if (", iStr s1, iStr " : ", pprType t1, iStr ") == (",
              iStr s2, iStr " : ", pprType t2, iStr ") {", iNewline,
              indentation, iIndent $ pprKExpr e1, iStr " } else { ", iNewline,
              indentation, iIndent $ pprKExpr e2, iNewline, iStr "} : ",
              pprType t ]
  pprKExpr (KEifLE (s1, t1) (s2, t2) e1 e2 t)         =
    iConcat [ iStr "if (", iStr s1, iStr " : ", pprType t1, iStr ") <= (",
              iStr s2, iStr " : ", pprType t2, iStr ") {", iNewline,
              indentation, iIndent $ pprKExpr e1, iStr " } else { ", iNewline,
              indentation, iIndent $ pprKExpr e2, iNewline, iStr "} : ",
              pprType t ]
  pprKExpr (KElet (s, t1) e1 e2 t)                    =
    iConcat [ iStr "let ", iStr s, iStr " : ", pprType t1, iStr " = ",
              pprKExpr e1, iNewline, iStr "in ", pprKExpr e2, iStr " : ",
              pprType t ]
  pprKExpr (KEletRec fd e t)                          =
    iConcat [ iStr "letrec ", pprFunDef fd, iNewline, iStr "in ", pprKExpr e,
              iStr " : ", pprType t ]
  pprKExpr (KEapply s ss t)                           =
    iConcat [ iInterleave (iStr " ") $ map (\(v, t') -> iConcat [ iStr "(",
              iStr v, iStr " : ", pprType t', iStr ")"]) $ s:ss, iStr " : ",
              pprType t ]
  pprKExpr (KEpair (s1, t1) (s2, t2) t)               =
    iConcat [ iStr "(", iStr s1, iStr " : ", pprType t1, iStr ", ", iStr s2,
              iStr " : ", pprType t2, iStr ") : ", pprType t ]
  pprKExpr (KEcons (s1, t1) (s2, t2) t)               =
    iConcat [ iStr s1, iStr " : ", pprType t1, iStr "::", iStr s2, iStr " : ",
              pprType t2, iStr " : ", pprType t ]
  pprKExpr (KEletPair (s1, t1) (s2, t2) (s3, t3) e t) =
    iConcat [ iStr "let (", iStr s1, iStr " : ", pprType t1, iStr ", ", iStr s2,
              iStr " : ", pprType t2, iStr ") = ", iStr s3, iStr " : ",
              pprType t3, iNewline, iStr "in ", pprKExpr e, iStr " : ",
              pprType t ]
  pprKExpr (KEletList (s1, t1) (s2, t2) (s3, t3) e t) =
    iConcat [ iStr "let ", iStr s1, iStr " : ", pprType t1, iStr "::", iStr s2,
              iStr " : ", pprType t2, iStr " = ", iStr s3, iStr " : ",
              pprType t3, iNewline, iStr "in ", pprKExpr e, iStr " : ",
              pprType t ]
  pprKExpr (KEhandle e1 e2 t)                         =
    iConcat [ pprKExpr e1, iNewline, iStr "rescue", iNewline, pprKExpr e2,
              iNewline, iStr " : ", pprType t ]
  pprKExpr (KEseq e1 e2 t)                            =
    iConcat [ pprKExpr e1, iStr "; ", pprKExpr e2, iStr " : ", pprType t ]
  pprKExpr (KEextFunApp s ss t)                       =
    iConcat [ iInterleave (iStr " ") $ map (\(v, t') -> iConcat [ iStr "(",
              iStr v, iStr " : ", pprType t', iStr ")"]) $ s:ss, iStr " : ",
              pprType t ]

  instance Show KExpr where
    show = show . pprKExpr

