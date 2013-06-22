module X86.Syntax (
  regs, fregs, regCl, regSp, regHp, failureLabel,
  Label(..), IdOrImm(..), Seq(..), Instr(..), FunDef(..), Program(..)
) where
  import X86.Syntax.Virtual
  --import X86.Syntax.Concrete

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
  regHp = "ocaml_min_heap_pointer"

  -- default staic failure label
  failureLabel :: Label
  failureLabel = L "_match_failure"
