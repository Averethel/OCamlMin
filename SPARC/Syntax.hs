module SPARC.Syntax where
  import Types

  data Label = L String deriving Eq

  instance Show Label where
    show (L s) = s

  data IdOrIimm =
      V String
    | C Integer
    deriving Eq

  instance Show IdOrIimm where
    show (V s) = s
    show (C n) = show n

  -- Instruction sequence
  data Seq =
      Ans Instr
    | Let String Type Instr Seq
    | Labeled Label Seq
    deriving (Eq, Show)

  -- SPARC assembly instructions
  data Instr =
      Inop
    | Iset Integer
    | IsetL Label
    | Imov String
    | Ineg String
    | Iadd String IdOrIimm
    | Isub String IdOrIimm
    | ISLL String IdOrIimm
    | Ild String  IdOrIimm
    | Ist String String IdOrIimm
    | IfMovD String
    | IfNegD String
    | IfAddD String String
    | IfSubD String String
    | IfMulD String String
    | IfDivD String String
    | IfModD String String
    | IldDF String IdOrIimm
    | IstDF String String IdOrIimm
    | Icomment String
    -- virtual instructions
    | IifEq String IdOrIimm Seq Seq
    | IifLE String IdOrIimm Seq Seq
    | IifGE String IdOrIimm Seq Seq
    | IifFEq String String Seq Seq
    | IifFLE String String Seq Seq
    -- closure address, integer arguments, and float arguments
    | IcallCls String [String] [String]
    | IcallDir Label [String] [String]
    | Isave String String
    | Irestore String
    | Ijump Label
    deriving (Eq, Show)

  data FunDef = FD {
    name  :: Label,
    args  :: [String],
    fargs :: [String],
    body  :: Seq,
    ret   :: Type
  } deriving (Eq, Show)

  data Program = P {
    toplevel :: [FunDef],
    main     :: Seq
  } deriving (Eq, Show)

  regs :: [String]
  regs =
    [ "%i2", "%i3", "%i4", "%i5",
      "%l0", "%l1", "%l2", "%l3", "%l4", "%l5", "%l6", "%l7",
      "%o0", "%o1", "%o2", "%o3", "%o4", "%o5" ]

  fregs :: [String]
  fregs =
    [ "%f0", "%f2", "%f4", "%f6", "%f8", "%f10",
      "%f12", "%f14", "%f16", "%f18", "%f20",
      "%f22", "%f24", "%f26", "%f28", "%f30" ]

  -- closure address
  regCl :: String
  regCl = last regs

  -- temporary for swap
  regSw :: String
  regSw = last . init $ regs

  -- stack pointer
  regSp :: String
  regSp = "%i0"

  -- heap pointer
  regHp :: String
  regHp = "%i1"

  -- return address
  regRa :: String
  regRa = "%o7"

  -- default staic failure label
  failureLabel :: Label
  failureLabel = L "_match_failure"
