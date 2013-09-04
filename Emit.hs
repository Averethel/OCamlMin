{-# LANGUAGE
    FlexibleContexts
    #-}

module Emit (emitProgram) where
  import X86.Syntax
  import X86.Utils

  import Types

  import CompilerState
  import Control.Exception.Base
  import Control.Monad.State
  import Data.Set (insert, member, intersection)
  import Data.List (partition)
  import Data.List.Utils


  save :: MonadState CompilerState m => String -> m ()
  save x = do
    s <- get
    let s' = if x `elem` stackMap s
              then s
              else s{ stackMap = stackMap s ++ [x] }
    put s'{ stackSet = x `insert` stackSet s' }

  savef :: MonadState CompilerState m => String -> m ()
  savef x = do
    s   <- get
    pad <- if length (stackMap s) `mod` 2 == 0
            then return []
            else do
              padId <- freshName $ "_tmp_" ++ genId Tint
              return [padId]
    let s' = if x `elem` stackMap s
              then s
              else s{ stackMap = stackMap s ++ pad ++ [x, x] }
    put s'{ stackSet = x `insert` stackSet s' }

  locate :: MonadState CompilerState m => String -> m [Integer]
  locate x = do
    m <- getC stackMap
    return $ loc m where
      loc :: [String] -> [Integer]
      loc []         = []
      loc (y:ys)
        | x == y    = 0 : map succ (loc ys)
        | otherwise = map succ $ loc ys

  offset :: MonadState CompilerState m => String -> m Integer
  offset x = do
    ls <- locate x
    return $ 4 * head ls

  stackSize :: MonadState CompilerState m => m Integer
  stackSize = do
    m <- getC stackMap
    return $ align (toInteger $ length m * 4)

  shuffle :: Eq a => a -> [(a, a)] -> [(a, a)]
  shuffle sw xys =
    let (_, xys') = partition (uncurry (==)) xys in
    case partition (\(_, y) -> y `hasKeyAL` xys') xys' of
      ([],          []) -> []
      ((x, y) : ls, []) ->
        (y, sw) : (x, y) :
          shuffle sw
            (map (\yz@(y', z) -> if y' == y then (sw, z) else yz) ls)
      (ls,        acyc) -> acyc ++ shuffle sw ls

  data CallType =
      Tail
    | NonTail String
    deriving Eq

  emitSeq :: MonadState CompilerState m => CallType -> Seq -> m [Instruction]
  emitSeq ct (Ans i)        =
    emitInstr ct i
  emitSeq ct (Let x _ i s)  = do
    s1' <- emitInstr (NonTail x) i
    s2' <- emitSeq ct s
    return $ s1' ++ s2'
  emitSeq ct (Labeled l s)  = do
    (h:t) <- emitSeq ct s
    return $ Lab l h : t

  tailUnit :: MonadState CompilerState m => Instr -> m [Instruction]
  tailUnit e = do
    s   <- freshName $ '_' : genId Tunit
    res <- emitInstr (NonTail s) e
    return $ res ++ [Ret]

  tailSimpleInt :: MonadState CompilerState m => Instr -> m [Instruction]
  tailSimpleInt i = do
    res <- emitInstr (NonTail $ head regs) i
    return $ res ++ [Ret]

  tailSimpleFloat :: MonadState CompilerState m => Instr -> m [Instruction]
  tailSimpleFloat i = do
      res <- emitInstr (NonTail $ head fregs) i
      return $ res ++ [Ret]

  restoreStacksetIf :: MonadState CompilerState m => StackSet -> m ()
  restoreStacksetIf stackSetB = do
    s <- get
    put s{ stackSet = stackSetB }

  tailIf :: MonadState CompilerState m => Seq -> Seq -> String ->
            (Label -> Instruction) -> m [Instruction]
  tailIf e1 e2 l bn = do
    bElse <- freshName $ l ++ "_else"
    stackSetBack <- getC stackSet
    b1 <- emitSeq Tail e1
    restoreStacksetIf stackSetBack
    (h2:b2) <- emitSeq Tail e2
    return $ bn (L bElse) : b1 ++ Lab (L bElse) h2 : b2

  nonTailIf :: MonadState CompilerState m => CallType -> Seq -> Seq -> String ->
               (Label -> Instruction) -> m [Instruction]
  nonTailIf dest e1 e2 b bn = do
    prefix <- freshName b
    let bElse = prefix ++ "_else"
    let bCont = prefix ++ "_cont"
    stackSetBack <- getC stackSet
    b1 <- emitSeq dest e1
    stackSet1 <- getC stackSet
    restoreStacksetIf stackSetBack
    (h2 : b2) <- emitSeq dest e2
    stackSet2 <- getC stackSet
    s <- get
    put s{ stackSet = stackSet1 `intersection` stackSet2 }
    return $ bn (L bElse) : b1 ++ JMP (L bCont) : Lab (L bElse) h2 : b2 ++ [Lab (L bCont) Nop]

  nonTailApp :: MonadState CompilerState m => Integer -> String -> Address -> m [Instruction]
  nonTailApp size a addr =
    return $ setSP ++ Call addr : restoreSP ++ rest
    where
      setSP
        | size > 0  = [AddL (AddrOf $ Const size) $ Var regSp]
        | otherwise = []
      restoreSP
        | size > 0  = [SubL (AddrOf $ Const size) $ Var regSp]
        | otherwise = []
      rest
        | a `elem` regs && a /= head regs   = [MovL (Var $ head regs) $ Var a]
        | a `elem` fregs && a /= head fregs = [MovSD (Var $ head fregs) $ Var a]
        | otherwise                         = []


  emitArgs :: MonadState CompilerState m => Maybe String -> [String] -> [String] ->
              m [Instruction]
  emitArgs mx ys zs = do
    size <- stackSize
    let sw  = Add size regSp
    let xrc = case mx of
              Nothing -> []
              Just x  -> [(Var x, Var regCl)]
    let (_, yrs) = foldl (\(i, yrgs) y -> (i + 1, (Var y, Var $ regs !! i) : yrgs)) (0, xrc) ys
    let rs = map (uncurry MovL) $ shuffle sw yrs
    let (_, zfrs) = foldl (\(d, zfrgs) z -> (d + 1, (Var z, Var $ fregs !! d) : zfrgs)) (0, []) zs
    let frs = concatMap (\(z, fr) -> [ MovSD z fr ]) $ shuffle sw zfrs
    return $ rs ++ frs

  emitInstr :: MonadState CompilerState m => CallType -> Instr -> m [Instruction]
  emitInstr (NonTail _) Inop     =
    return []
  emitInstr (NonTail x) (Iset i) =
    return [MovL (AddrOf $ Const i) $ Var x]
  emitInstr (NonTail x) (IsetL (L y)) =
    return [MovL (AddrOf $ Var y) $ Var x]
  emitInstr (NonTail x) (Imov y)
    | x /= y    =
      return [MovL (Var y) $ Var x]
    | otherwise =
      return []
  emitInstr (NonTail x) (Ineg y)
    | x /= y    =
      return [MovL (Var y) $ Var x,
              NegL $ Var x]
    | otherwise =
      return [NegL $ Var x]
  emitInstr (NonTail x) (Iadd y z')
    | V x == z' =
      return [AddL (Var y) $ Var x]
    | x /= y    =
      return [MovL (Var y) $ Var x,
              AddL (toAddr z') $ Var x]
    | otherwise =
      return [AddL (toAddr z') $ Var x]
  emitInstr (NonTail x) (Isub y z')
    | V x == z' =
      return [SubL (Var y) $ Var x,
              NegL $ Var x]
    | x /= y    =
      return [MovL (Var y) $ Var x,
              SubL (toAddr z') $ Var x]
    | otherwise =
      return [SubL (toAddr z') $ Var x]
  emitInstr (NonTail x) (ISdiv y z')
    | x /= y    =
      return [MovL (Var y) $ Var x,
              IDivL (toAddr z') $ Var x]
    | otherwise =
      return [IDivL (toAddr z') $ Var x]
  emitInstr (NonTail x) (ISmul y z')
    | V x == z' =
      return [IMulL (Var y) $ Var x]
    | x /= y    =
      return [MovL (Var y) $ Var x,
              IMulL (toAddr z') $ Var x]
    | otherwise =
      return [IMulL (toAddr z') $ Var x]
  emitInstr (NonTail x) (Ild y (V z) i) =
    return [MovL (MulAdd y z i) $ Var x]
  emitInstr (NonTail x) (Ild y (C j) i) =
    return [MovL (Add (j*i) y) $ Var x]
  emitInstr (NonTail _) (Ist x y (V z) i) =
    return [MovL (Var x) $ MulAdd y z i]
  emitInstr (NonTail _) (Ist x y (C j) i) =
    return [MovL (Var x) $ Add (j * i) y]
  emitInstr (NonTail x) (IfMovD y)
    | x /= y    =
      return [MovSD (Var y) $ Var x]
    | otherwise =
      return []
  emitInstr (NonTail x) (IfNegD y)
    | x /= y    =
      return [MovSD (Var y) $ Var x,
              XorPD (Var fneg) $ Var x]
    | otherwise =
      return [XorPD (Var fneg) $ Var x]
  emitInstr (NonTail x) (IfAddD y z)
    | x /= y    =
      return [MovSD (Var y) $ Var x,
              AddSD (Var z) $ Var x]
    | otherwise =
      return [AddSD (Var z) $ Var x]
  emitInstr (NonTail x) (IfSubD y z)
    | x /= y    =
      return [MovSD (Var y) $ Var x,
              SubSD (Var z) $ Var x]
    | otherwise =
      return [SubSD (Var z) $ Var x]
  emitInstr (NonTail x) (IfMulD y z)
    | x /= y    =
      return [MovSD (Var y) $ Var x,
              MulSD (Var z) $ Var x]
    | otherwise =
      return [MulSD (Var z) $ Var x]
  emitInstr (NonTail x) (IfDivD y z)
    | x /= y    =
      return [MovSD (Var y) $ Var x,
              DivSD (Var z) $ Var x]
    | otherwise =
      return [DivSD (Var z) $ Var x]
  emitInstr (NonTail x) (IldDF y (V z) i) =
    return [MovSD (MulAdd y z i) $ Var x]
  emitInstr (NonTail x) (IldDF y (C j) i) =
    return [MovSD (Add (j * i) y) $ Var x]
  emitInstr (NonTail _) (IstDF x y (V z) i) =
    return [MovSD (Var x) $ MulAdd y z i]
  emitInstr (NonTail _) (IstDF x y (C j) i) =
    return [MovSD (Var x) $ Add (j * i) y]
  emitInstr (NonTail _) (Icomment s) =
    return [Comment s]
  emitInstr (NonTail _) (Isave x y)
    | x `elem` regs  = do
      ss <- getC stackSet
      if   not (y `member` ss)
      then do
        save y
        y' <- offset y
        return [MovL (Var x) $ Add y' regSp]
      else
        return []
    | x `elem` fregs = do
      ss <- getC stackSet
      if   not (y `member` ss)
      then do
        savef y
        y' <- offset y
        return [MovSD (Var x) $ Add y' regSp]
      else
        return []
    | otherwise      = do
      ss <- getC stackSet
      assert (y `member` ss) $ return []
  emitInstr (NonTail x) (Irestore y)
    | x `elem` regs = do
      y' <- offset y
      return [MovL (Add y' regSp) $ Var x]
    | otherwise     = assert (x `elem` fregs) $ do
      y' <- offset y
      return [MovSD (Add y' regSp) $ Var x]
  emitInstr Tail e@Inop          =
    tailUnit e
  emitInstr Tail e@(Ist{})       =
    tailUnit e
  emitInstr Tail e@(IstDF{})     =
    tailUnit e
  emitInstr Tail e@(Icomment _)  =
    tailUnit e
  emitInstr Tail e@(Isave{})     =
    tailUnit e
  emitInstr Tail e@(Iset _)      =
    tailSimpleInt e
  emitInstr Tail e@(IsetL _)     =
    tailSimpleInt e
  emitInstr Tail e@(Imov _)      =
    tailSimpleInt e
  emitInstr Tail e@(Ineg _)      =
    tailSimpleInt e
  emitInstr Tail e@(Iadd {})     =
    tailSimpleInt e
  emitInstr Tail e@(Isub {})     =
    tailSimpleInt e
  emitInstr Tail e@(ISmul {})    =
    tailSimpleInt e
  emitInstr Tail e@(ISdiv {})    =
    tailSimpleInt e
  emitInstr Tail e@(ISLL {})     =
    tailSimpleInt e
  emitInstr Tail e@(Ild {})      =
    tailSimpleInt e
  emitInstr Tail e@(IfMovD _)    =
    tailSimpleFloat e
  emitInstr Tail e@(IfNegD _)    =
    tailSimpleFloat e
  emitInstr Tail e@(IfAddD {})   =
    tailSimpleFloat e
  emitInstr Tail e@(IfSubD {})   =
    tailSimpleFloat e
  emitInstr Tail e@(IfMulD {})   =
    tailSimpleFloat e
  emitInstr Tail e@(IfDivD {})   =
    tailSimpleFloat e
  emitInstr Tail e@(IldDF {})    =
    tailSimpleFloat e
  emitInstr Tail e@(Irestore x)  = do
    l   <- locate x
    res <- case l of
            [_]    -> emitInstr (NonTail $ head regs) e
            [i, j] -> assert (i + 1 ==j) $ emitInstr (NonTail $ head fregs) e
            _      -> assert False $ return []
    return $ res ++ [Ret]
  emitInstr Tail (IifEq x y' e1 e2) = do
    res <- tailIf e1 e2 "je" JNE
    return $ CmpL (toAddr y') (Var x) : res
  emitInstr Tail (IifLE x y' e1 e2) = do
    res <- tailIf e1 e2 "jle" JG
    return $ CmpL (toAddr y') (Var x) : res
  emitInstr Tail (IifGE x y' e1 e2) = do
    res <- tailIf e1 e2 "jge" JL
    return $ CmpL (toAddr y') (Var x) : res
  emitInstr Tail (IifFEq x y e1 e2) = do
    res <- tailIf e1 e2 "je" JNE
    return $ ComiSD (Var y) (Var x) : res
  emitInstr Tail (IifFLE x y e1 e2) = do
    res <- tailIf e1 e2 "jbe" JA
    return $ ComiSD (Var y) (Var x) : res
  emitInstr (NonTail z) (IifEq x y' e1 e2) = do
    res <- nonTailIf (NonTail z) e1 e2 "je" JNE
    return $ CmpL (toAddr y') (Var x) : res
  emitInstr (NonTail z) (IifLE x y' e1 e2) = do
    res <- nonTailIf (NonTail z) e1 e2 "jle" JG
    return $ CmpL (toAddr y') (Var x) : res
  emitInstr (NonTail z) (IifGE x y' e1 e2) = do
    res <- nonTailIf (NonTail z) e1 e2 "jge" JL
    return $ CmpL (toAddr y') (Var x) : res
  emitInstr (NonTail z) (IifFEq x y e1 e2) = do
    res <- nonTailIf (NonTail z) e1 e2 "je" JNE
    return $ ComiSD (Var y) (Var x) : res
  emitInstr (NonTail z) (IifFLE x y e1 e2) = do
    res <- nonTailIf (NonTail z) e1 e2 "jbe" JA
    return $ ComiSD (Var y) (Var x) : res
  emitInstr Tail (IcallCls x ys zs) = do
    res <- emitArgs (Just x) ys zs
    return $ res ++ [Jump $ ValueOf regCl]
  emitInstr Tail (IcallDir l ys zs) = do
    res <- emitArgs Nothing ys zs
    return $ res ++ [JMP l]
  emitInstr (NonTail a) (IcallCls x ys zs) = do
    res <- emitArgs (Just x) ys zs
    size <- stackSize
    app <- nonTailApp size a $ Var regCl
    return $ res ++ app
  emitInstr (NonTail a) (IcallDir (L x) ys zs) = do
    res <- emitArgs Nothing ys zs
    size <- stackSize
    app <- nonTailApp size a $ Var x
    return $ res ++ app
  emitInstr _ (Ijump l) =
    return [JMP l]

  emitFunDef :: MonadState CompilerState m => FunDef -> m Function
  emitFunDef (FD { name = l, body = e }) = do
    s <- get
    put s{ stackSet = emptyStackSet, stackMap = emptyStackMap }
    b <- emitSeq Tail e
    return F{ label = l, fBody = b }

  emitProgram :: (MonadIO m, MonadState CompilerState m) => Program -> m Prog
  emitProgram (P funDefs e) = do
    liftIO $ putStrLn "generating assembly...@."
    -- after adding float data section needs to be here
    dfs <- mapM emitFunDef funDefs
    s <- get
    put s{ stackSet = emptyStackSet, stackMap = emptyStackMap }
    e' <- emitSeq (NonTail $ head regs) e
    let pushes = map (PushL . Var . ('%' : )) pushRegs
    let movs   = [MovL (Add 32 "%%esp") $ Var regSp,
                  MovL (Add 36 "%%esp") $ Var $ head $ regs,
                  MovL (Var $ head regs) $ Var regHp]
    let pops   = map (PopL . Var . ('%' : )) $ reverse pushRegs
    return Pg { functions = dfs, mainFun = pushes ++ movs ++ e' ++ pops ++ [Ret] }
