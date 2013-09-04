module X86.Syntax.Concrete where
  import X86.Syntax.Virtual (Label)

  import Utils.Iseq hiding (indentation)

  data Address =
      Var String
    | Const Integer
    | AddrOf Address
    | ValueOf String
    | Add Integer String
    | MulAdd String String Integer
    deriving Eq

  pprAddress :: Address -> Iseq
  pprAddress (Var s)        = iStr s
  pprAddress (Const i)      = iStr $ show i
  pprAddress (AddrOf a)     = iConcat [ iStr "$", iStr $  show a ]
  pprAddress (ValueOf s)    = iConcat [ iStr "*(", iStr s, iStr ")" ]
  pprAddress (Add i s)      = iConcat [ iStr $ show i, iStr "(", iStr s, iStr ")" ]
  pprAddress (MulAdd s r i) = iConcat [ iStr "(", iStr s, iStr ",", iStr r, iStr ",", iStr $ show i, iStr ")"]

  instance Show Address where
    show = show . pprAddress

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

  indentation :: Iseq
  indentation = iStr "\t"

  pprInstruction :: Instruction -> Iseq
  pprInstruction (MovL a1 a2)   =
    iConcat [indentation, iStr "movl ", pprAddress a1, iStr ", ", pprAddress a2]
  pprInstruction (MovSD a1 a2)  =
    iConcat [indentation, iStr "movsd ", pprAddress a1, iStr ", ", pprAddress a2]
  pprInstruction (PushL a)      =
    iConcat [indentation, iStr "pushl ", pprAddress a]
  pprInstruction (PopL a)       =
    iConcat [indentation, iStr "popl ", pprAddress a]
  pprInstruction (NegL  a)      =
    iConcat [indentation, iStr "negl ", pprAddress a]
  pprInstruction (AddL  a1 a2)  =
    iConcat [indentation, iStr "addl ", pprAddress a1, iStr ", ", pprAddress a2]
  pprInstruction (SubL  a1 a2)  =
    iConcat [indentation, iStr "subl ", pprAddress a1, iStr ", ", pprAddress a2]
  pprInstruction (IDivL a1 a2)  =
    iConcat [indentation, iStr "idivl ", pprAddress a1, iStr ", ", pprAddress a2]
  pprInstruction (IMulL a1 a2)  =
    iConcat [indentation, iStr "imull ", pprAddress a1, iStr ", ", pprAddress a2]
  pprInstruction (AddSD a1 a2)  =
    iConcat [indentation, iStr "addsd ", pprAddress a1, iStr ", ", pprAddress a2]
  pprInstruction (SubSD a1 a2)  =
    iConcat [indentation, iStr "subsd ", pprAddress a1, iStr ", ", pprAddress a2]
  pprInstruction (MulSD a1 a2)  =
    iConcat [indentation, iStr "mulsd ", pprAddress a1, iStr ", ", pprAddress a2]
  pprInstruction (DivSD a1 a2)  =
    iConcat [indentation, iStr "divsd ", pprAddress a1, iStr ", ", pprAddress a2]
  pprInstruction (XorPD a1 a2)  =
    iConcat [indentation, iStr "xorpd ", pprAddress a1, iStr ", ", pprAddress a2]
  pprInstruction (CmpL a1 a2)   =
    iConcat [indentation, iStr "cmpl ", pprAddress a1, iStr ", ", pprAddress a2]
  pprInstruction (ComiSD a1 a2) =
    iConcat [indentation, iStr "comisd ", pprAddress a1, iStr ", ", pprAddress a2]
  pprInstruction (JMP l)        =
    iConcat [indentation, iStr "jmp ", iStr $ show l]
  pprInstruction (JNE l)        =
    iConcat [indentation, iStr "jne ", iStr $ show l]
  pprInstruction (JG  l)        =
    iConcat [indentation, iStr "jg ", iStr $ show l]
  pprInstruction (JL  l)        =
    iConcat [indentation, iStr "jl ", iStr $ show l]
  pprInstruction (JA  l)        =
    iConcat [indentation, iStr "ja ", iStr $ show l]
  pprInstruction (Jump a)       =
    iConcat [indentation, iStr "jmp ", pprAddress a]
  pprInstruction (Call a)       =
    iConcat [indentation, iStr "call ", pprAddress a]
  pprInstruction (Comment s)    =
    iConcat [indentation, iStr "# ", iStr s]
  pprInstruction Ret            =
    iConcat [indentation, iStr "ret"]
  pprInstruction Nop            =
    iConcat [indentation, iStr "nop"]
  pprInstruction (Lab l i)      =
    iConcat [iStr $ show l, iStr ":", iNewline, iIndent $ pprInstruction i]

  instance Show Instruction where
    show = show . pprInstruction

    showList ls _ = show . iInterleave iNewline . map (iIndent . pprInstruction) $ ls

  data Function = F {
    label :: Label,
    fBody :: [Instruction]
  }

  instance Show Function where
    show (F l b) = show l ++ ":\n" ++ show b

    showList []     _ = ""
    showList (f:fs) a = show f ++ '\n' : showList fs a

  data Prog = Pg {
    functions :: [Function],
    mainFun   :: [Instruction]
  }

  instance Show Prog where
    show (Pg fs m) =
      ".data\n" ++
      ".align\t8\n" ++
      -- output of data section goes here
      ".text\n" ++
      show fs ++
      "\n.global OCamlMin_START\nOCamlMin_START\n" ++
      "OCamlMin_START:\n" ++
      "OCamlMin_START:\n" ++
      show m
