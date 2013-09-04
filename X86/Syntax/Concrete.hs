module X86.Syntax.Concrete where
  import X86.Syntax.Virtual (Label)
  data Address =
      Var String
    | Const Integer
    | AddrOf Address
    | ValueOf String
    | Add Integer String
    | MulAdd String String Integer
    deriving Eq

  data Instruction =
      MovL Address Address
    | MovSD Address Address
    | PushL Address
    | PopL Address

    | NegL  Address
    | AddL  Address Address
    | SubL  Address Address
    | IDivL Address Address
    | IMulL Address Address

    | AddSD Address Address
    | SubSD Address Address
    | MulSD Address Address
    | DivSD Address Address
    | XorPD Address Address

    | CmpL Address Address
    | ComiSD Address Address
    | JMP Label
    | JNE Label
    | JG  Label
    | JL  Label
    | JA  Label
    | Jump Address
    | Call Address

    | Comment String

    | Ret
    | Nop

    | Lab Label Instruction
    deriving Eq

  data Function = F {
    label :: Label,
    fBody :: [Instruction]
  }

  data Prog = Pg {
    functions :: [Function],
    mainFun   :: [Instruction]
  }