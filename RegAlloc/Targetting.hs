module RegAlloc.Targetting where
  import X86.Syntax
  import X86.Utils
  import Types

  import Control.Exception.Base

  targetIf :: String -> String -> Type -> Seq -> Seq -> (Bool, [String])
  targetIf src dest t e1 e2 = (c1 && c2, rs1 ++ rs2) where
    (c1, rs1) = target src dest t e1
    (c2, rs2) = target src dest t e2

  target' :: String -> String -> Type -> Instr -> (Bool, [String])
  target' src dest t (Imov x)
    | x == src && isReg dest            =
      assert (t /= Tunit && t /= Tfloat) (False, [dest])
  target' src dest t (IfMovD x)
    | x == src && isReg dest            =
      assert (t /= Tfloat) (False, [dest])
  target' src dest t (IifEq _ _ e1 e2)  =
    targetIf src dest t e1 e2
  target' src dest t (IifLE _ _ e1 e2)  =
    targetIf src dest t e1 e2
  target' src dest t (IifGE _ _ e1 e2)  =
    targetIf src dest t e1 e2
  target' src dest t (IifFEq _ _ e1 e2) =
    targetIf src dest t e1 e2
  target' src dest t (IifFLE _ _ e1 e2) =
    targetIf src dest t e1 e2
  target' src _ _ (IcallCls x ys zs)    =
    (True,  targetArgs src regs 0 ys ++
            targetArgs src fregs 0 zs ++
            [regCl | x == src])
  target' src _ _ (IcallDir _ ys zs)    =
    (True,  targetArgs src regs 0 ys ++
            targetArgs src fregs 0 zs)
  target' _ _ _ _                       =
    (False, [])

  targetArgs :: String -> [String] -> Int -> [String] -> [String]
  targetArgs _ _ _ []            = []
  targetArgs src allR n (y : ys)
    | src == y                   = allR !! n : targetArgs src allR (n + 1) ys
  targetArgs src allR n (_ : ys) = targetArgs src allR (n + 1) ys

  target :: String -> String -> Type -> Seq -> (Bool, [String])
  target src dest t (Ans i)       =
    target' src dest t i
  target src dest r (Let x t i e) =
    if c1
    then (True, rs1)
    else (c2, rs1 ++ rs2)
    where
      (c1, rs1) = target' src x t i
      (c2, rs2) = target src dest r e
  target src dest t (Labeled _ e) =
    target src dest t e

  source :: Type -> Seq -> [String]
  source t (Ans e)       = sourceInstr t e
  source t (Let _ _ _ s) = source t s
  source t (Labeled _ s) = source t s

  sourceInstr :: Type -> Instr -> [String]
  sourceInstr _ (Imov x)            = [ x ]
  sourceInstr _ (Ineg x)            = [ x ]
  sourceInstr _ (Iadd x (C _))      = [ x ]
  sourceInstr _ (Isub x _)          = [ x ]
  sourceInstr _ (ISmul x _)         = [ x ]
  sourceInstr _ (ISdiv x _)         = [ x ]
  sourceInstr _ (IfMovD x)          = [ x ]
  sourceInstr _ (IfNegD x)          = [ x ]
  sourceInstr _ (IfSubD x _)        = [ x ]
  sourceInstr _ (IfDivD x _)        = [ x ]
  sourceInstr _ (Iadd x (V y))      = [ x, y ]
  sourceInstr _ (IfAddD x y)        = [ x, y ]
  sourceInstr _ (IfMulD x y)        = [ x, y ]
  sourceInstr t (IifEq _ _ e1 e2)   = source t e1 ++ source t e2
  sourceInstr t (IifLE _ _ e1 e2)   = source t e1 ++ source t e2
  sourceInstr t (IifGE _ _ e1 e2)   = source t e1 ++ source t e2
  sourceInstr t (IifFEq _ _ e1 e2)  = source t e1 ++ source t e2
  sourceInstr t (IifFLE _ _ e1 e2)  = source t e1 ++ source t e2
  sourceInstr t (IcallCls{})        =
    case t of
      Tunit                        -> []
      Tfloat                       -> [ head fregs ]
      _                            -> [ head regs ]
  sourceInstr t (IcallDir{})        =
    case t of
      Tunit                        -> []
      Tfloat                       -> [ head fregs ]
      _                            -> [ head regs ]
  sourceInstr _ _                   = []
