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

  coFregs :: [(String, String)]
  coFregs =
    [ ("%f0",  "%f1"),  ("%f2",  "%f3"),  ("%f4",  "%f5"),  ("%f6",  "%f7"),
      ("%f8",  "%f9"),  ("%f10", "%f11"), ("%f12", "%f13"), ("%f14", "%f15"),
      ("%f16", "%f17"), ("%f18", "%f19"), ("%f20", "%f21"), ("%f22", "%f23"),
      ("%f24", "%f25"), ("%f26", "%f27"), ("%f28", "%f29"), ("%f30", "%f31") ]

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
