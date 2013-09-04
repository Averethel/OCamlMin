module X86.Syntax (
  regs, fregs, regCl, regSp, regHp, failureLabel, toAddr, fneg, pushRegs,
  Label(..), IdOrImm(..), Seq(..), Instr(..), FunDef(..), Program(..),
  Address(..), Instruction(..), Function(..), Prog(..)
) where
  import X86.Syntax.Virtual
  import X86.Syntax.Concrete

  regs :: [String]
  regs = [ "%eax", "%ebx", "%ecx", "%edx", "%esi", "%edi" ]

  fregs :: [String]
  fregs = [ "%xmm0", "%xmm1", "%xmm2", "%xmm3", "%xmm4", "%xmm5", "%xmm6", "%xmm7" ]

  -- closure address
  regCl :: String
  regCl = last regs

  -- stack pointer
  regSp :: String
  regSp = "%ebp"

  -- heap pointer
  regHp :: String
  regHp = "OcamlMin_HEAPPOINTER"

  -- default staic failure label
  failureLabel :: Label
  failureLabel = L "_match_failure"

  toAddr :: IdOrImm -> Address
  toAddr (V s) = Var s
  toAddr (C n) = AddrOf $ Const n

  -- name of float negation variable
  fneg :: String
  fneg = "OCamlMin_FNEG"

  pushRegs :: [String]
  pushRegs = regs ++ [regSp]
