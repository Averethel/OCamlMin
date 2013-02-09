module Syntax.Pattern where
  import Syntax.Constant

  import Utils.Iseq

  data Pattern =
      Pwildcard
    | Pvar      String
    | Pconst    Constant
    | Ptuple    [Pattern]
    | Pcons     Pattern Pattern
    deriving Eq

  isAtomicPattern :: Pattern -> Bool
  isAtomicPattern Pwildcard  = True
  isAtomicPattern (Pvar _)   = True
  isAtomicPattern (Pconst _) = True
  isAtomicPattern _          = False

  pprAPattern :: Pattern -> Iseq
  pprAPattern p
    | isAtomicPattern p = pprPattern p
    | otherwise         = iStr "(" `iAppend` pprPattern p `iAppend` iStr ")"

  pprPattern :: Pattern -> Iseq
  pprPattern Pwildcard     =
    iStr "_"
  pprPattern (Pvar v)      =
    iStr v
  pprPattern (Pconst c)    =
    pprConstant c
  pprPattern (Ptuple ps)   =
    iConcat [ iStr "(",  iInterleave (iStr ", ") $
              map pprAPattern ps, iStr ")" ]
  pprPattern (Pcons p1 p2) =
    pprAPattern p1 `iAppend` iStr " :: " `iAppend` pprAPattern p2

  instance Show Pattern where
    show = show . pprPattern
