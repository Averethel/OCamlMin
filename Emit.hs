{-# LANGUAGE
    FlexibleContexts
    #-}

module Emit where

  import SPARC.Syntax
  import SPARC.Utils

  import Types

  import Counters
  import Control.Exception.Base
  import Control.Monad.State
  import Data.Set (insert, member, intersection)
  import Data.List (partition)
  import Data.List.Utils

  save :: MonadState Counter m => String -> m ()
  save x = do
    s <- get
    let s' = if x `elem` stackMap s
              then s
              else s{ stackMap = stackMap s ++ [x] }
    put s'{ stackSet = x `insert` stackSet s' }

  savef :: MonadState Counter m => String -> m ()
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

  loc :: String -> [String] -> [Integer]
  loc _ []     = []
  loc x (y:ys)
    | x == y      = 0 : map succ (loc x ys)
    | otherwise   = map succ $ loc x ys

  locate :: MonadState Counter m => String -> m [Integer]
  locate x = do
    m <- getC stackMap
    return $ loc x m

  offset :: MonadState Counter m => String -> m Integer
  offset x = do
    ls <- locate x
    return $ 4 * head ls

  stackSize :: MonadState Counter m => m Integer
  stackSize = do
    m <- getC stackMap
    return $ align (toInteger (length m + 1) * 4)

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

  emitSeq :: MonadState Counter m => CallType -> Seq -> m [Instruction]
  emitSeq ct (Ans i)        =
    emitInstr ct i
  emitSeq ct (Let x _ i s)  = do
    s1' <- emitInstr (NonTail x) i
    s2' <- emitSeq ct s
    return $ s1' ++ s2'
  emitSeq ct (Labeled l s)  = do
    (h:t) <- emitSeq ct s
    return $ Lab l h : t

  tailUnit :: MonadState Counter m => Instr -> m [Instruction]
  tailUnit e = do
    s   <- freshName $ '_' : genId Tunit
    res <- emitInstr (NonTail s) e
    return $ res ++ [RetL, Nop]

  tailSimpleInt :: MonadState Counter m => Instr -> m [Instruction]
  tailSimpleInt i = do
    res <- emitInstr (NonTail $ head regs) i
    return $ res ++ [RetL, Nop]

  tailSimpleFloat :: MonadState Counter m => Instr -> m [Instruction]
  tailSimpleFloat i = do
      res <- emitInstr (NonTail $ head fregs) i
      return $ res ++ [RetL, Nop]

  restoreStacksetIf :: MonadState Counter m => StackSet -> m ()
  restoreStacksetIf stackSetB = do
    s <- get
    put s{ stackSet = stackSetB }

  tailIf :: MonadState Counter m => Seq -> Seq -> String ->
            (Label -> Instruction) -> m [Instruction]
  tailIf e1 e2 l bn = do
    bElse <- freshName $ l ++ "_else"
    stackSetBack <- getC stackSet
    b1 <- emitSeq Tail e1
    restoreStacksetIf stackSetBack
    b2 <- emitSeq Tail e2

    return $ bn (L bElse) : Nop : b1 ++ Lab (L bElse) Nop : b2

  nonTailIf :: MonadState Counter m => CallType -> Seq -> Seq -> String ->
               (Label -> Instruction) -> m [Instruction]
  nonTailIf dest e1 e2 b bn = do
    bElse <- freshName $ b ++ "_else"
    bCont <- freshName $ b ++ "_cont"
    stackSetBack <- getC stackSet
    b1 <- emitSeq dest e1
    stackSet1 <- getC stackSet
    restoreStacksetIf stackSetBack
    b2 <- emitSeq dest e2
    stackSet2 <- getC stackSet
    s <- get
    put s{ stackSet = stackSet1 `intersection` stackSet2 }
    return $ bn (L bElse) : Nop : b1 ++ B (L bCont) : Lab (L bElse) Nop : b2 ++ [Lab (L bCont) Nop]

  nonTailApp :: MonadState Counter m => Integer -> String -> m [Instruction]
  nonTailApp size a = do
    let rest
          | a `elem` regs && a /= head regs   = [ Mov (head regs) a ]
          | a `elem` fregs && a /= head fregs = [ FmovS (head fregs) a, FmovS (coFreg $ head fregs) $ coFreg a ]
          | otherwise                         = []
    return $ Add regSp (C size) regSp : Sub regSp (C size) regSp : Ld (V regSp :+: C (size - 4)) regRa : rest

  emitArgs :: MonadState Counter m => Maybe String -> [String] -> [String] ->
              m [Instruction]
  emitArgs mx ys zs = do
    let xrc = case mx of
              Nothing -> []
              Just x  -> [(x, regCl)]
    let (_, yrs) = foldl (\(i, yrgs) y -> (i + 1, (y, regs !! i) : yrgs)) (0, xrc) ys
    let rs = map (uncurry Mov) $ shuffle regSw yrs
    let (_, zfrs) = foldl (\(d, zfrgs) z -> (d + 1, (z, fregs !! d) : zfrgs)) (0, []) zs
    let frs = concatMap (\(z, fr) -> [ FmovS z fr, FmovS (coFreg z) $ coFreg fr ]) $ shuffle regFsw zfrs
    return $ rs ++ frs

  emitInstr :: MonadState Counter m => CallType -> Instr -> m [Instruction]
  emitInstr (NonTail _) Inop     =
    return []
  emitInstr (NonTail x) (Iset i) =
    return [Set (C i) x]
  emitInstr (NonTail x) (IsetL (L y)) =
    return [Set (V y) x]
  emitInstr (NonTail x) (Imov y)
    | x == y    =
      return []
    | otherwise =
      return [Mov y x]
  emitInstr (NonTail x) (Ineg y)     =
    return [Neg y x]
  emitInstr (NonTail x) (Iadd y z')  =
    return [Add y z' x]
  emitInstr (NonTail x) (Isub y z')  =
    return [Sub y z' x]
  emitInstr (NonTail x) (ISdiv y z') =
    return [Sdiv y z' x]
  emitInstr (NonTail x) (ISmul y z') =
    return [Smul y z' x]
  emitInstr (NonTail x) (ISLL y z')  =
    return [SLL y z' x]
  emitInstr (NonTail x) (Ild y z') =
    return [Ld (V y :+: z') x]
  emitInstr (NonTail _) (Ist x y z') =
   return [St x $ V y :+: z']
  emitInstr (NonTail x) (IfMovD y)
    | x == y    =
      return []
    | otherwise =
      return [ FmovS y x,
               FmovS (coFreg y) $ coFreg x ]
  emitInstr (NonTail x) (IfNegD y)
    | x /= y    =
      return [ FnegS y x,
               FmovS (coFreg y) $ coFreg x ]
    | otherwise =
      return [FnegS y x]
  emitInstr (NonTail x) (IfAddD y z) =
    return [FaddD y z x]
  emitInstr (NonTail x) (IfSubD y z) =
    return [FsubD y z x]
  emitInstr (NonTail x) (IfMulD y z) =
    return [FmulD y z x]
  emitInstr (NonTail x) (IfDivD y z) =
    return [FdivD y z x]
  emitInstr (NonTail x) (IldDF y z') =
    return [Ldd (V y :+: z') x]
  emitInstr (NonTail _) (IstDF x y z') =
    return [Std x $ V y :+: z']
  emitInstr (NonTail _) (Icomment s) =
    return [Comment s]
  emitInstr (NonTail _) (Isave x y)
    | x `elem` regs  = do
      ss <- getC stackSet
      if   not (y `member` ss)
      then do
        save y
        y' <- offset y
        return [St x $ V regSp :+: C y']
      else do
        savef y
        y' <- offset y
        return [Std x $ V regSp :+: C y']
    | otherwise     = do
      ss <- getC stackSet
      assert (y `member` ss) $ return []
  emitInstr (NonTail x) (Irestore y)
    | x `elem` regs = do
        y' <- offset y
        return [Ld (V regSp :+: C y') x]
    | otherwise     = do
        y' <- offset y
        assert (x `elem` regs) $
          return [Ldd (V regSp :+: C y') x]
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
  emitInstr Tail e@(ISmul {})     =
    tailSimpleInt e
  emitInstr Tail e@(ISdiv {})     =
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
    return $ res ++ [RetL, Nop]
  emitInstr Tail (IifEq x y' e1 e2) = do
    res <- tailIf e1 e2 "be" BNE
    return $ Cmp x y' : res
  emitInstr Tail (IifLE x y' e1 e2) = do
    res <- tailIf e1 e2 "ble" BG
    return $ Cmp x y' : res
  emitInstr Tail (IifGE x y' e1 e2) = do
    res <- tailIf e1 e2 "bge" BL
    return $ Cmp x y' : res
  emitInstr Tail (IifFEq x y e1 e2) = do
    res <- tailIf e1 e2 "fbe" FBNE
    return $ FcmpD x y : Nop : res
  emitInstr Tail (IifFLE x y e1 e2) = do
    res <- tailIf e1 e2 "fble" FBG
    return $ FcmpD x y : Nop : res
  emitInstr (NonTail z) (IifEq x y' e1 e2) = do
    res <- nonTailIf (NonTail z) e1 e2 "be" BNE
    return $ Cmp x y' : res
  emitInstr (NonTail z) (IifLE x y' e1 e2) = do
    res <- nonTailIf (NonTail z) e1 e2 "ble" BG
    return $ Cmp x y' : res
  emitInstr (NonTail z) (IifGE x y' e1 e2) = do
    res <- nonTailIf (NonTail z) e1 e2 "bge" BL
    return $ Cmp x y' : res
  emitInstr (NonTail z) (IifFEq x y e1 e2) = do
    res <- nonTailIf (NonTail z) e1 e2 "fbe" FBNE
    return $ FcmpD x y : Nop : res
  emitInstr (NonTail z) (IifFLE x y e1 e2) = do
    res <- nonTailIf (NonTail z) e1 e2 "fble" FBG
    return $ FcmpD x y : Nop : res
  emitInstr Tail (IcallCls x ys zs) = do
    res <- emitArgs (Just x) ys zs
    return $ res ++ [Ld (V regCl :+: C 0) regSw, Jmp regSw, Nop]
  emitInstr Tail (IcallDir l ys zs) = do
    res <- emitArgs Nothing ys zs
    return $ res ++ [B l, Nop]
  emitInstr (NonTail a) (IcallCls x ys zs) = do
    res <- emitArgs (Just x) ys zs
    size <- stackSize
    app <- nonTailApp size a
    return $ res ++ St regRa (V regSp :+: C (size - 4)) : Ld (V regCl :+: C 0) regSw : Call regSw : app
  emitInstr (NonTail a) (IcallDir l ys zs) = do
    res <- emitArgs Nothing ys zs
    size <- stackSize
    app <- nonTailApp size a
    return $ res ++ St regRa (V regSp :+: C (size - 4)) : CallL l : app
  emitInstr _ (Ijump l) =
    return [B l]

  emitFunDef :: MonadState Counter m => FunDef -> m Function
  emitFunDef (FD { name = l, body = e }) = do
    s <- get
    put s{ stackSet = emptyStackSet, stackMap = emptyStackMap }
    b <- emitSeq Tail e
    return F{ label = l, fBody = b }

  emitProgram :: (MonadIO m, MonadState Counter m) => Program -> m Prog
  emitProgram (P funDefs e) = do
    liftIO $ putStrLn "generating assembly...@."
    -- after adding float data section needs to be here
    dfs <- mapM emitFunDef funDefs
    s <- get
    put s{ stackSet = emptyStackSet, stackMap = emptyStackMap }
    e' <- emitSeq (NonTail "%g0") e
    return Pg { functions = dfs, mainFun = Save regSp (C $ -112) regSp : e' ++ [Ret, Restore]}
