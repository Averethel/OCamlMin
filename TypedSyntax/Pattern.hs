module TypedSyntax.Pattern where
  import TypedSyntax.Constant
  import Types

  import Utils.Iseq

  data TypedPattern =
      TPwildcard Type
    | TPvar      String Type
    | TPconst    TypedConstant
    | TPpair     TypedPattern TypedPattern Type
    | TPcons     TypedPattern TypedPattern Type
    deriving Eq

  typeOfTypedPattern :: TypedPattern -> Type
  typeOfTypedPattern (TPwildcard t)   = t
  typeOfTypedPattern (TPvar _ t)      = t
  typeOfTypedPattern (TPconst (_, t)) = t
  typeOfTypedPattern (TPpair _ _ t)   = t
  typeOfTypedPattern (TPcons _ _ t)   = t

  applySubstTP :: TypedPattern -> Subst -> TypedPattern
  applySubstTP (TPwildcard t)   s = TPwildcard $ t `applySubst` s
  applySubstTP (TPvar n t)      s = TPvar n $ t `applySubst` s
  applySubstTP (TPconst (c, t)) s = TPconst (c, t `applySubst` s)
  applySubstTP (TPpair t1 t2 t) s = TPpair (t1 `applySubstTP` s)
                                          (t2 `applySubstTP` s) $
                                          t `applySubst` s
  applySubstTP (TPcons t1 t2 t) s = TPcons (t1 `applySubstTP` s)
                                          (t2 `applySubstTP` s) $
                                          t `applySubst` s

  isAtomicTypedPattern :: TypedPattern -> Bool
  isAtomicTypedPattern (TPwildcard _) = True
  isAtomicTypedPattern (TPvar _ _)    = True
  isAtomicTypedPattern (TPconst _)    = True
  isAtomicTypedPattern _              = False

  hasSubPatterns :: TypedPattern -> Bool
  hasSubPatterns (TPpair p1 p2 _) = not (isAtomicTypedPattern p1) ||
                                    not (isAtomicTypedPattern p2)
  hasSubPatterns (TPcons p1 p2 _) = not (isAtomicTypedPattern p1) ||
                                    not (isAtomicTypedPattern p2)
  hasSubPatterns _                = False

  pprTypedAPattern :: TypedPattern -> Iseq
  pprTypedAPattern p
    | isAtomicTypedPattern p = pprTypedPattern p
    | otherwise              = iStr "(" `iAppend` pprTypedPattern p `iAppend`
                               iStr ")"

  pprTypedPattern :: TypedPattern -> Iseq
  pprTypedPattern (TPwildcard t)   =
    iConcat [ iStr "_ : ", pprType t ]
  pprTypedPattern (TPvar v t)      =
    iConcat [ iStr v, iStr " : ", pprType t ]
  pprTypedPattern (TPconst c)      =
    pprTypedConstant c
  pprTypedPattern (TPpair p1 p2 t) =
    iConcat [ iStr "(",  iInterleave (iStr ", ") $
              map pprTypedAPattern [p1, p2], iStr ") : ", pprType t ]
  pprTypedPattern (TPcons p1 p2 t) =
    iConcat [ pprTypedAPattern p1, iStr " :: ", pprTypedAPattern p2, iStr " : ",
              pprType t ]

  instance Show TypedPattern where
    show = show . pprTypedPattern
