module Syntax.BinaryPrim where
  import Utils.Iseq

  data BinaryPrim =
      BPeq
    | BPlt
    | BPgt
    | BPor
    | BPand
    | BPadd
    | BPsub
    | BPmult
    | BPdiv
    | BPmod
    | BPassign
    deriving Eq

  pprBinaryPrim :: BinaryPrim -> Iseq
  pprBinaryPrim BPeq      = iStr "=="
  pprBinaryPrim BPlt      = iStr "<"
  pprBinaryPrim BPgt      = iStr ">"
  pprBinaryPrim BPor      = iStr "or"
  pprBinaryPrim BPand     = iStr "and"
  pprBinaryPrim BPadd     = iStr "+"
  pprBinaryPrim BPsub     = iStr "-"
  pprBinaryPrim BPmult    = iStr "*"
  pprBinaryPrim BPdiv     = iStr "/"
  pprBinaryPrim BPmod     = iStr "%"
  pprBinaryPrim BPassign  = iStr ":="

  instance Show BinaryPrim where
    show = show . pprBinaryPrim
