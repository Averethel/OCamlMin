module Syntax.UnaryPrim where
  import Utils.Iseq

  data UnaryPrim =
      UPnot
    | UPref
    | UPderef
    | UPminus
    deriving Eq

  pprUnaryPrim :: UnaryPrim -> Iseq
  pprUnaryPrim UPnot          = iStr "not"
  pprUnaryPrim UPref          = iStr "!"
  pprUnaryPrim UPderef        = iStr "&"
  pprUnaryPrim UPminus        = iStr "-"

  instance Show UnaryPrim where
    show = show . pprUnaryPrim
