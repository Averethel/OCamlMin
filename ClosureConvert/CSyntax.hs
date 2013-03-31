module ClosureConvert.CSyntax where
  import Utils.Iseq

  import Types

  import Data.Set hiding (map)

  data Label = L String deriving Eq

  instance Show Label where
    show (L s) = "_L_" ++ s

  data Closure = C {
    entry  :: (Label, Type),
    actFvs :: [(String, Type)]
  } deriving Eq

  pprClosure :: Closure -> Iseq
  pprClosure c = iConcat [ iStr "{ ", iStr . show . fst . entry $ c, iStr " : ",
                           pprType . snd . entry $ c,  iStr ", [ ",
                           iInterleave (iStr ", ") $
                           map (\(x, t) -> iConcat [ iStr x, iStr " : ",
                           pprType t ]) (actFvs c), iStr " }" ]

  instance Show Closure where
    show = show . pprClosure

  data CExpr =
    -- constants
      CEunit Type
    | CEnil Type
    | CEint Integer Type
    -- unary primitives
    | CEneg (String, Type) Type
    | CEload (String, Type) Type
    -- binary primitives
    | CEadd (String, Type) (String, Type) Type
    | CEsub (String, Type) (String, Type) Type
    | CEmult (String, Type) (String, Type) Type
    | CEdiv (String, Type) (String, Type) Type
    | CEmod (String, Type) (String, Type) Type
    | CEstore (String, Type) (String, Type) Type
    --
    | CEvar String Type
    | CEerror String Type
    | CEifEq (String, Type) (String, Type) CExpr CExpr Type
    | CEifLE (String, Type) (String, Type) CExpr CExpr Type
    | CElet String Type CExpr CExpr Type
    | CEmakeClj String Closure CExpr Type
    | CEappClj (String, Type) [(String, Type)] Type
    | CEappDir (Label, Type) [(String, Type)] Type
    | CEpair (String, Type) (String, Type) Type
    | CEcons (String, Type) (String, Type) Type
    | CEletPair (String, Type) (String, Type) (String, Type) CExpr Type
    | CEletList (String, Type) (String, Type) (String, Type) CExpr Type
    | CEhandle CExpr CExpr Type
    | CEseq CExpr CExpr Type
    deriving Eq

  freeVars :: CExpr -> Set (String, Type)
  freeVars (CEneg s1 _)              =
    singleton s1
  freeVars (CEload s1 _)             =
    singleton s1
  freeVars (CEadd s1 s2 _)           =
    fromList [s1, s2]
  freeVars (CEsub s1 s2 _)           =
    fromList [s1, s2]
  freeVars (CEmult s1 s2 _)          =
    fromList [s1, s2]
  freeVars (CEdiv s1 s2 _)           =
    fromList [s1, s2]
  freeVars (CEmod s1 s2 _)           =
    fromList [s1, s2]
  freeVars (CEstore s1 s2 _)         =
    fromList [s1, s2]
  freeVars (CEvar s1 t)              =
    singleton (s1, t)
  freeVars (CEifEq s1 s2 e1 e2 _)    =
    s1 `insert` (s2 `insert` freeVars e1 `union` freeVars e2)
  freeVars (CEifLE s1 s2 e1 e2 _)    =
    s1 `insert` (s2 `insert` freeVars e1 `union` freeVars e2)
  freeVars (CElet s1 t e1 e2 _)      =
    (s1, t) `delete` (freeVars e1 `union` freeVars e2)
  freeVars (CEmakeClj s1 clj e t)    =
    (s1, t) `delete` (fromList (actFvs clj) `union` freeVars e)
  freeVars (CEappClj s1 ss _)        =
    fromList $ s1 : ss
  freeVars (CEappDir _ ss _)         =
    fromList ss
  freeVars (CEpair s1 s2 _)          =
    fromList [s1, s2]
  freeVars (CEcons s1 s2 _)          =
    fromList [s1, s2]
  freeVars (CEletPair s1 s2 s3 e _)  =
    s3 `insert` freeVars e \\ fromList [s1, s2]
  freeVars (CEletList s1 s2 s3 e _)  =
    s3 `insert` freeVars e \\ fromList [s1, s2]
  freeVars (CEhandle e1 e2 _)        =
    freeVars e1 `union` freeVars e2
  freeVars (CEseq e1 e2 _)           =
    freeVars e1 `union` freeVars e2
  freeVars _                         =
    empty

  pprCExpr :: CExpr -> Iseq
  pprCExpr (CEunit t)                                 =
    iStr "() : " `iAppend` pprType t
  pprCExpr (CEnil t)                                  =
    iStr "[] : " `iAppend` pprType t
  pprCExpr (CEint n t)                                =
    iConcat [ iStr . show $ n, iStr " : ", pprType t ]
  pprCExpr (CEneg (s, t1) t)                          =
    iConcat [ iStr "-", iStr s, iStr " : ", pprType t1, iStr " : ", pprType t ]
  pprCExpr (CEload (s, t1) t)                         =
    iConcat [ iStr "&", iStr s, iStr " : ", pprType t1, iStr " : ", pprType t ]
  pprCExpr (CEadd (s1, t1) (s2, t2) t)                =
    iConcat [ iStr s1, iStr " : ", pprType t1, iStr " + ", iStr s2, iStr " : ",
              pprType t2, iStr " : ", pprType t ]
  pprCExpr (CEsub (s1, t1) (s2, t2) t)                =
    iConcat [ iStr s1, iStr " : ", pprType t1, iStr " - ", iStr s2, iStr " : ",
              pprType t2, iStr " : ", pprType t ]
  pprCExpr (CEmult (s1, t1) (s2, t2) t)               =
    iConcat [ iStr s1, iStr " : ", pprType t1, iStr " * ", iStr s2, iStr " : ",
              pprType t2, iStr " : ", pprType t ]
  pprCExpr (CEdiv (s1, t1) (s2, t2) t)                =
    iConcat [ iStr s1, iStr " : ", pprType t1, iStr " / ", iStr s2, iStr " : ",
              pprType t2, iStr " : ", pprType t ]
  pprCExpr (CEmod (s1, t1) (s2, t2) t)                =
    iConcat [ iStr s1, iStr " : ", pprType t1, iStr " % ", iStr s2, iStr " : ",
              pprType t2, iStr " : ", pprType t ]
  pprCExpr (CEstore (s1, t1) (s2, t2) t)              =
    iConcat [ iStr s1, iStr " : ", pprType t1, iStr " := ", iStr s2, iStr " : ",
              pprType t2, iStr " : ", pprType t ]
  pprCExpr (CEvar s t)                                =
    iConcat [ iStr s, iStr " : ", pprType t ]
  pprCExpr (CEerror s t)                              =
    iConcat [ iStr s, iStr " : ", pprType t ]
  pprCExpr (CEifEq (s1, t1) (s2, t2) e1 e2 t)         =
    iConcat [ iStr "if ", iStr s1, iStr " : ", pprType t1,  iStr " == ",
              iStr s2, iStr " : ", pprType t2, iStr "{", iNewline, indentation,
              iIndent $ pprCExpr e1, iStr " } else { ", iNewline, indentation,
              iIndent $ pprCExpr e2, iNewline, iStr "} : ", pprType t ]
  pprCExpr (CEifLE (s1, t1) (s2, t2) e1 e2 t)         =
    iConcat [ iStr "if ", iStr s1, iStr " : ", pprType t1, iStr " <= ",
              iStr s2, iStr " : ", pprType t2, iStr "{", iNewline, indentation,
              iIndent $ pprCExpr e1, iStr " } else { ", iNewline, indentation,
              iIndent $ pprCExpr e2, iNewline, iStr "} : ", pprType t ]
  pprCExpr (CElet s t1 e1 e2 t)                       =
    iConcat [ iStr "let ", iStr s, iStr " : ", pprType t1, iStr " = ",
              pprCExpr e1, iNewline, iStr "in ", pprCExpr e2, iStr " : ",
              pprType t ]
  pprCExpr (CEmakeClj s c e t)                        =
    iConcat [ iStr "make_closure( ", iStr s, pprClosure c, pprCExpr e,
                                              iStr " ) : ", pprType t ]
  pprCExpr (CEappClj (s, t1) ss t)                    =
    iConcat [ iStr s, iStr " : ", pprType t1, iStr "@( ", iInterleave
              (iStr ", ") $ map (\(x, tp) -> iConcat [ iStr x, iStr " : ",
              pprType tp ]) ss, iStr " ) : ", pprType t ]
  pprCExpr (CEappDir (s, t1) ss t)                    =
    iConcat [ iStr . show $ s, iStr " : ", pprType t1, iStr "@( ", iInterleave
              (iStr ", ") $ map (\(x, tp) -> iConcat [ iStr x, iStr " : ",
              pprType tp ]) ss, iStr " ) : ", pprType t ]
  pprCExpr (CEpair (s1, t1) (s2, t2) t)               =
    iConcat [ iStr "(", iStr s1, iStr " : ", pprType t1, iStr ", ", iStr s2,
              iStr " : ", pprType t2, iStr ") : ", pprType t ]
  pprCExpr (CEcons (s1, t1) (s2, t2) t)               =
    iConcat [ iStr s1, iStr " : ", pprType t1, iStr "::", iStr s2, iStr " : ",
              pprType t2, iStr " : ", pprType t ]
  pprCExpr (CEletPair (s1, t1) (s2, t2) (s3, t3) e t) =
    iConcat [ iStr "let (", iStr s1, iStr " : ", pprType t1, iStr ", ", iStr s2,
              iStr " : ", pprType t2, iStr ") = ", iStr s3, iStr " : ",
              pprType t3, iNewline, iStr "in ", pprCExpr e, iStr " : ",
              pprType t ]
  pprCExpr (CEletList (s1, t1) (s2, t2) (s3, t3) e t) =
    iConcat [ iStr "let ", iStr s1, iStr " : ", pprType t1, iStr "::", iStr s2,
              iStr " : ", pprType t2, iStr " = ", iStr s3, iStr " : ",
              pprType t3, iNewline, iStr "in ", pprCExpr e, iStr " : ",
              pprType t ]
  pprCExpr (CEhandle e1 e2 t)                         =
    iConcat [ pprCExpr e1, iNewline, iStr "rescue", iNewline, pprCExpr e2,
              iStr " : ", pprType t ]
  pprCExpr (CEseq e1 e2 t)                            =
    iConcat [ pprCExpr e1, iStr "; ", pprCExpr e2, iStr " : ", pprType t ]

  instance Show CExpr where
    show = show . pprCExpr

  data FunDef = FD {
    name      :: (Label, Type),
    args      :: [(String, Type)],
    formalFvs :: [(String, Type)],
    body      :: CExpr
  } deriving Eq

  pprFunDef :: FunDef -> Iseq
  pprFunDef fd =
    iConcat [ iStr . show . fst . name $ fd, iStr " ", iInterleave (iStr " ") $
              map (\(x, t) -> iConcat [ iStr x, iStr " : ", pprType t ]) $
              args fd, iStr " =", iNewline, indentation, iIndent . pprCExpr .
              body $ fd, iNewline, iStr "FVs = ", iInterleave (iStr ", ") $
              map (\(x, t) -> iConcat [ iStr x, iStr " : ", pprType t ]) $
              formalFvs fd, iNewline, pprType . snd . name $ fd ]

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


