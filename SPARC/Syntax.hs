module SPARC.Syntax (
  regs, fregs, regCl, regSw, regSp, regHp, regRa, failureLabel,
  Label(..), IdOrIimm(..), Seq(..), Instr(..), FunDef(..), Program(..)
) where
  import SPARC.Virtual.Syntax

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
