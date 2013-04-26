module SPARC.Syntax where
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
    | Let String Instr Seq
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
    body  :: Seq
  } deriving (Eq, Show)

  data Program = P {
    toplevel :: [FunDef],
    main     :: Seq
  } deriving (Eq, Show)
