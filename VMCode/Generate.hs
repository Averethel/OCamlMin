{-# LANGUAGE
  FlexibleContexts
  #-}

module VMCode.Generate where
  import ClosureConvert.CSyntax hiding (C)
  import CompilerState
  import Types

  import X86.Syntax
  import X86.Utils hiding (freeVars)

  import Control.Exception.Base
  import Control.Monad.State
  import qualified Data.Set as S

  toLabel :: (ClosureConvert.CSyntax.Label, Type) -> X86.Syntax.Label
  toLabel (ClosureConvert.CSyntax.L s, _) = X86.Syntax.L s

  classify :: MonadState CompilerState m =>
              [(String, Type)] -> b ->
              (b -> String -> Type -> m b) ->
              m b
  classify xts ini addi =
    foldM (\acc (x, t) ->
      case t of
        Tunit -> return acc
        _     -> addi acc x t)
      ini
      xts

  expand :: MonadState CompilerState m =>
            [(String, Type)] -> (Integer, Seq) ->
            (String -> Type -> Integer -> Seq -> m Seq) -> m (Integer, Seq)
  expand xts ini addi =
    classify xts ini
      (\(offset, acc) x t -> do {
        r <- addi x t offset acc;
        return (offset + 4, r)})

  pairHeader :: Integer
  pairHeader = 4096

  listHeader :: Integer
  listHeader = 4097

  letBlock :: (MonadState CompilerState m) => X86.Syntax.Label ->
              (String, Type) -> (String, Type) -> (String, Type) -> CExpr ->
              m Seq
  letBlock l s1 s2 s3 e = do
    tg <- freshName "_t"
    let fvs = freeVars e
    e' <- translateExpr l e
    (_, load) <- expand [(tg, Tint), s1, s2] (0, e')
          (\x t offset load ->
            return $
              if   not (S.member x $ S.map fst fvs)
              then load
              else Let x t (Ild (fst s3) (C offset) 1) load)
    return load

  translateExpr :: (MonadState CompilerState m) =>
                   X86.Syntax.Label -> CExpr -> m Seq
  translateExpr _ (CEunit _) =
    return $ Ans Inop
  translateExpr _ (CEnil _) =
    return $ Ans $ Iset 0 -- empty list is 0
  translateExpr _ (CEint n _) =
    return $ Ans $ Iset n
  translateExpr _ (CEneg (s, _) _) =
    return $ Ans $ Ineg s
  translateExpr _ (CEload s (Tref t)) =
    return $
      case t of
        Tunit -> Ans Inop
        _     -> Ans $ Ild (fst s) (C 0) 4
  translateExpr _ (CEadd (s1, _) (s2, _) _) =
    return $ Ans $ Iadd s1 $ V s2
  translateExpr _ (CEsub (s1, _) (s2, _) _) =
    return $ Ans $ Isub s1 $ V s2
  translateExpr _ (CEmult (s1, _) (s2, _) _) =
    return $ Ans $ ISmul s1 $ V s2
  translateExpr _ (CEdiv (s1, _) (s2, _) _) =
    return $ Ans $ ISdiv s1 $ V s2
  translateExpr _ (CEstore (s1, Tref t) s2 _) =
    return $
      case t of
        Tunit -> Ans Inop
        _     -> Ans $ Ist s1 (fst s2) (C 0) 4
  translateExpr _ (CEvar _ Tunit) =
    return $ Ans Inop
  translateExpr _ (CEvar s _) =
    return $ Ans $ Imov s
  -- Set error register here with the message
  translateExpr l (CEerror _ _) =
    return $ Ans $ Ijump l
  translateExpr l (CEifEq (s1, _) (s2, _) e1 e2 _) = do
    e1' <- translateExpr l e1
    e2' <- translateExpr l e2
    return $ Ans $ IifEq s1 (V s2) e1' e2'
  translateExpr l (CEifLE (s1, _) (s2, _) e1 e2 _) = do
    e1' <- translateExpr l e1
    e2' <- translateExpr l e2
    return $ Ans $ IifLE s1 (V s2) e1' e2'
  translateExpr l (CElet s t e1 e2 _) = do
    e1' <- translateExpr l e1
    e2' <- translateExpr l e2
    return $ X86.Utils.concat e1' s t e2'
  translateExpr l (CEmakeClj s cl e t)      = do
    e' <- translateExpr l e
    let f y _ off = instrSeq (Ist y s (C off) 1)
    (offset, storeFv) <- expand (actFvs cl) (4, e') f
    z  <- freshName "_c"
    sq <- instrSeq (Ist z s (C 0) 1) storeFv
    return $ Let s t (Imov regHp) $
        Let regHp Tint (Iadd regHp $ C $ align offset) $
          Let z Tint (IsetL $ toLabel $ entry cl) sq
  translateExpr _ (CEappClj s ss _)         =
    return $ Ans $ IcallCls (fst s) (map fst ss) [] -- no floats at the moment
  translateExpr _ (CEappDir l ss _)         =
    return $ Ans $ IcallDir (toLabel l) (map fst ss) [] -- no floats at the moment
  translateExpr _ (CEpair s1 s2 t)          = do
    tg <- freshName "_t"
    y  <- freshName "_p"
    let f x _ off = instrSeq (Ist x y (C off) 1)
    (offset, store) <- expand [(tg, Tint), s1, s2] (0, Ans $ Imov y) f
    return $ Let tg Tint (Iset pairHeader) $
      Let y t (Imov regHp) $
        Let regHp Tint (Iadd regHp $ C $ align offset) store
  translateExpr _ (CEcons s1 s2 t)          = do
    tg <- freshName "_t"
    y  <- freshName "_l"
    let f x _ off = instrSeq (Ist x y (C off) 1)
    (offset, store) <- expand [(tg, Tint), s1, s2] (0, Ans $ Imov y) f
    return $ Let tg Tint (Iset listHeader) $
      Let y t (Imov regHp) $
        Let regHp Tint (Iadd regHp $ C $ align offset) store
  translateExpr l (CEletPair s1 s2 s3 e _)  =
    letBlock l s1 s2 s3 e
  translateExpr l (CEletList s1 s2 s3 e _)  =
    letBlock l s1 s2 s3 e
  translateExpr l (CEhandle e1 e2 _)        = do
    exl <- nextErrorLabel
    e1' <- translateExpr exl e1
    e2' <- translateExpr l e2
    hdl <- nextHandledLabel
    s1  <- buildSeq e1' $ Ans $ Ijump hdl
    s2  <- buildSeq (Labeled exl e2') $ Labeled hdl $ Ans Inop
    buildSeq s1 s2
  translateExpr l (CEseq e1 e2 _)           = do
    e1' <- translateExpr l e1
    e2' <- translateExpr l e2
    buildSeq e1' e2'
  translateExpr l e =
    assert False $ translateExpr l e

  translateFunDef :: (MonadState CompilerState m) =>
                     X86.Syntax.Label -> ClosureConvert.CSyntax.FunDef ->
                     m X86.Syntax.FunDef
  translateFunDef l fd = do
    e'        <- translateExpr l $ ClosureConvert.CSyntax.body fd
    (_, load) <- expand
      (formalFvs fd)
      (4, e')
      (\z t offset load -> return $ Let z t (Ild regCl (C offset) 1) load)
    let Tfun _ t2 = snd . ClosureConvert.CSyntax.name $ fd
    return X86.Syntax.FD {
      X86.Syntax.name  = toLabel . ClosureConvert.CSyntax.name $ fd,
      X86.Syntax.args  = map fst $ ClosureConvert.CSyntax.args fd,
      X86.Syntax.fargs = [], -- no floats at the moment
      X86.Syntax.body  = load,
      X86.Syntax.ret   = t2
    }

  translateProgram :: (MonadState CompilerState m) =>
                      ClosureConvert.CSyntax.Program ->
                      m X86.Syntax.Program
  translateProgram p = do
    fds'  <- mapM (translateFunDef failureLabel) $ definitions p
    main' <- translateExpr failureLabel $ ClosureConvert.CSyntax.main p
    return X86.Syntax.P {
      X86.Syntax.toplevel = fds',
      X86.Syntax.main     = main'
    }

