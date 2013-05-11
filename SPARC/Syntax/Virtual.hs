module SPARC.Syntax.Virtual where
  import Types

  data Label = L String deriving Eq

  instance Show Label where
    show (L s) = s

  data IdOrImm =
      V String
    | C Integer
    deriving Eq

  instance Show IdOrImm where
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
    | Iadd String IdOrImm
    | Isub String IdOrImm
    | ISmul String IdOrImm
    | ISdiv String IdOrImm
    | ISLL String IdOrImm
    | Ild String  IdOrImm
    | Ist String String IdOrImm
    | IfMovD String
    | IfNegD String
    | IfAddD String String
    | IfSubD String String
    | IfMulD String String
    | IfDivD String String
    | IldDF String IdOrImm
    | IstDF String String IdOrImm
    | Icomment String
    -- virtual instructions
    | IifEq String IdOrImm Seq Seq
    | IifLE String IdOrImm Seq Seq
    | IifGE String IdOrImm Seq Seq
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
