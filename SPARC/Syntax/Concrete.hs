module SPARC.Syntax.Concrete where
  import SPARC.Syntax.Virtual

  data Address =
      Const IdOrImm
    | IdOrImm :+: IdOrImm
    | IdOrImm :-: IdOrImm
    deriving Eq


  data Instruction =
      Comment String
    | Add String IdOrImm String
    | FaddD String String String
    | Sub String IdOrImm String
    | FsubD String String String
    | Smul String IdOrImm String
    | FmulD String String String
    | Sdiv String IdOrImm String
    | FdivD String String String
    | Neg String String
    | FnegS String String
    | SLL String IdOrImm String

    | Cmp String IdOrImm
    | Jmp String
    | FcmpD String String
    | B Label
    | BG Label
    | FBG Label
    | BL Label
    | BNE Label
    | FBNE Label

    | Call String
    | CallL Label

    | Ld Address String
    | Ldd Address String
    | St String Address
    | Std String Address
    | Set IdOrImm String
    | Mov String String
    | FmovS String String

    | Save String IdOrImm String

    | Nop
    | RetL
    | Ret
    | Restore
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