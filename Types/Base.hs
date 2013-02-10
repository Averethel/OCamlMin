module Types.Base where
  import Utils.Iseq

  data Type =
      Tint
    | Tbool
    | Tunit
    | Tvar    String
    | Tlist   Type
    | Tref    Type
    | Tpair   Type Type
    | Tfun    [Type] Type
    deriving Eq

  isAtomicType :: Type -> Bool
  isAtomicType (Tfun _ _) = False
  isAtomicType (Tlist _)  = False
  isAtomicType (Tref _)   = False
  isAtomicType _          = True

  pprAType :: Type -> Iseq
  pprAType t
    | isAtomicType t  = pprType t
    | otherwise       = iStr "(" `iAppend` pprType t `iAppend` iStr ")"

  pprType :: Type -> Iseq
  pprType Tint          = iStr "int"
  pprType Tbool         = iStr "bool"
  pprType Tunit         = iStr "unit"
  pprType (Tvar v)      = iStr v
  pprType (Tlist t)     = pprAType t `iAppend` iStr " list"
  pprType (Tref t)      = pprType t `iAppend` iStr " ref"
  pprType (Tpair t1 t2) = iConcat [ iStr "(", iInterleave (iStr ", ") $
                                    map pprAType [t1, t2], iStr ")" ]
  pprType (Tfun ts t)   = iConcat [ iInterleave (iStr " -> ") $ map pprAType ts,
                                    iStr " -> ", pprType t ]

  instance Show Type where
    show = show . pprType
