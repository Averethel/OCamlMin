module Syntax.Constant where
  import Utils.Iseq

  data Constant =
      Cint  Integer
    | Cbool Bool
    | Cnil
    | Cunit
    deriving Eq

  pprConstant :: Constant -> Iseq
  pprConstant (Cint n)  = iStr . show $ n
  pprConstant (Cbool b) = iStr . show $ b
  pprConstant Cnil      = iStr "[]"
  pprConstant Cunit     = iStr "()"

  instance Show Constant where
    show n = show . pprConstant $ n
