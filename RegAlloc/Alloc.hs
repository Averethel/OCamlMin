{-# LANGUAGE
  FlexibleContexts
  #-}

module RegAlloc.Alloc where
  import Prelude hiding (catch)

  import RegAlloc.Env
  import RegAlloc.Targetting
  import SPARC.Syntax
  import SPARC.Utils
  import Types

  import Counters
  import Control.Exception.Base (assert)
  import Control.Monad.Exception.Synchronous hiding (assert)
  import Control.Monad.State
  import Data.List hiding (insert)
  import Data.List.Utils
  import Data.Maybe (fromJust)
  import Data.Set hiding (foldl, map)

  data AllocResult =
      Alloc String -- allocated register
    | Spill String -- spilled variable


  alloc :: (MonadState Counter m, MonadIO m) => String -> Type -> Seq -> Env ->
           String -> Type -> m AllocResult
  alloc _    _ _    regenv x Tunit =
    assert (not $ x `hasKeyAL` regenv) $ return $ Alloc "%g0"
  alloc dest t cont regenv x tp
    | isReg x                      =
      assert (not $ x `hasKeyAL` regenv) $ return $ Alloc x
    | otherwise                    = assert (not $ x `hasKeyAL` regenv) $ do
      let allRegs = case tp of
                    Tfloat -> fregs
                    _      -> regs
      let fvs = freeVars cont
      let (_, prefer) = target x dest t cont
      let live = foldl (\l y -> if isReg y then y `insert` l else case y `lookup` regenv of
            Nothing -> l
            Just r  -> r `insert` l) empty fvs
      let r = find (\rg -> not $ rg `member` live) $ prefer ++ allRegs
      case r of
        Just reg -> do
          liftIO $ putStrLn $ "Allocated " ++ show x ++ " to " ++ show r ++ "."
          return $ Alloc reg
        Nothing  -> do
          liftIO $ putStrLn $ "Register allocation failed for " ++ show x ++ "."
          let Just y = find (\e -> not (isReg y) && (case e `lookup` regenv of {
            Nothing -> False;
            Just z  -> z `elem` allRegs})) $ reverse fvs
          liftIO $ putStrLn $ "spilling " ++ show y ++ " from " ++ show (fromJust $ y `lookup` regenv) ++ "."
          return $ Spill y

  regAllocSeq :: (MonadState Counter m, MonadIO m) => String -> Type -> Seq ->
                 Env -> Seq -> m (Seq, Env)
  regAllocSeq dest t cont regenv (Ans i)        =
    regAllocInstrAndRestore dest t cont regenv i
  regAllocSeq dest t cont regenv (Let x tx i s) =
    assert (not $ x `hasKeyAL` regenv) $ do
      let cont'      = SPARC.Utils.concat s dest t cont
      (s1, regenv') <- regAllocInstrAndRestore x tx cont' regenv i
      allocRes      <- alloc dest t cont' regenv' x tx
      case allocRes of
        Spill y -> do
          let Just r      = y `lookup` regenv'
          (s2, regenv'') <- regAllocSeq dest t cont (addToAL (regenv' `delFromAL` y) x r) s
          let save = case y `lookup` regenv of
                        Nothing -> Inop
                        Just rg -> Isave rg y
          seq' <- instrSeq save $ SPARC.Utils.concat s1 r tx s2
          return (seq', regenv'')
        Alloc r -> do
          (s2, regenv'') <- regAllocSeq dest t cont (addToAL regenv' x r) s
          return (SPARC.Utils.concat s1 r tx s2, regenv'')
  regAllocSeq dest t cont regenv (Labeled l s)  = do
    (s', regenv') <- regAllocSeq dest t cont regenv s
    return (Labeled l s', regenv')

  regAllocInstrAndRestore :: (MonadState Counter m, MonadIO m) => String ->
                              Type -> Seq -> Env -> Instr -> m (Seq, Env)
  regAllocInstrAndRestore dest t cont regenv i = do
    r <- runExceptionalT $ regAllocInstr dest t cont regenv i
    case r of
      Success res       -> return res
      Exception (x, tx) -> do
        liftIO $ putStrLn $ "Restoring " ++ show x ++ "."
        regAllocSeq dest t cont regenv $ Let x tx (Irestore x) $ Ans i

  regAllocSingleArg :: (MonadState Counter m, MonadIO m) => Type ->
                        (String -> Instr) -> String -> Env ->
                        ExceptionalT (String, Type) m (Seq, Env)
  regAllocSingleArg rType constr x regenv =
    envFind x rType regenv >>= (\s -> return (Ans $ constr s, regenv))

  regAllocDoubleArgInt :: (MonadState Counter m, MonadIO m) =>
                          (String -> IdOrImm -> Instr) -> String -> IdOrImm ->
                          Env -> ExceptionalT (String, Type) m (Seq, Env)
  regAllocDoubleArgInt constr x y regenv = do
    x' <- envFind x Tint regenv
    y' <- envFind' y regenv
    return (Ans $ constr x' y', regenv)

  regAllocDoubleArgFloat :: (MonadState Counter m, MonadIO m) =>
                            (String -> String -> Instr) -> String -> String ->
                            Env -> ExceptionalT (String, Type) m (Seq, Env)
  regAllocDoubleArgFloat constr x y regenv = do
    x' <- envFind x Tfloat regenv
    y' <- envFind y Tfloat regenv
    return (Ans $ constr x' y', regenv)

  regAllocTripleArg :: (MonadState Counter m, MonadIO m) => Type -> Type ->
                        (String -> String -> IdOrImm -> Instr) -> String ->
                        String -> IdOrImm -> Env ->
                        ExceptionalT (String, Type) m (Seq, Env)
  regAllocTripleArg xType yType constr x y z regenv = do
    x' <- envFind x xType regenv
    y' <- envFind y yType regenv
    z' <- envFind' z regenv
    return (Ans $ constr x' y' z', regenv)

  regAllocInstr :: (MonadState Counter m, MonadIO m) => String -> Type -> Seq ->
                    Env -> Instr -> ExceptionalT (String, Type) m (Seq, Env)
  regAllocInstr _    _ _    regenv (Imov x)             =
    regAllocSingleArg Tint Imov x regenv
  regAllocInstr _    _ _    regenv (Ineg x)             =
    regAllocSingleArg Tint Ineg x regenv
  regAllocInstr _    _ _    regenv (Iadd x y)           =
   regAllocDoubleArgInt Iadd x y regenv
  regAllocInstr _    _ _    regenv (Isub x y)           =
    regAllocDoubleArgInt Isub x y regenv
  regAllocInstr _    _ _    regenv (ISLL x y)           =
    regAllocDoubleArgInt ISLL x y regenv
  regAllocInstr _    _ _    regenv (Ild x y)            =
    regAllocDoubleArgInt Ild x y regenv
  regAllocInstr _    _ _    regenv (Ist x y z)          =
    regAllocTripleArg Tint Tint Ist x y z regenv
  regAllocInstr _    _ _    regenv (IfMovD x)           =
    regAllocSingleArg Tfloat IfMovD x regenv
  regAllocInstr _    _ _    regenv (IfNegD x)           =
    regAllocSingleArg Tfloat IfNegD x regenv
  regAllocInstr _    _ _    regenv (IfAddD x y)         =
    regAllocDoubleArgFloat IfAddD x y regenv
  regAllocInstr _    _ _    regenv (IfSubD x y)         =
    regAllocDoubleArgFloat IfSubD x y regenv
  regAllocInstr _    _ _    regenv (IfMulD x y)         =
    regAllocDoubleArgFloat IfMulD x y regenv
  regAllocInstr _    _ _    regenv (IfDivD x y)         =
    regAllocDoubleArgFloat IfDivD x y regenv
  regAllocInstr _    _ _    regenv (IldDF x y)          =
    regAllocDoubleArgInt IldDF x y regenv
  regAllocInstr _    _ _    regenv (IstDF x y z)        =
    regAllocTripleArg Tfloat Tint IstDF x y z regenv
  regAllocInstr dest t cont regenv (IifEq x y e1 e2)  =
    regAllocIf dest t cont regenv (\e1' e2' -> do {
      x' <- envFind x Tint regenv;
      y' <- envFind' y regenv;
      return $ IifEq x' y' e1' e2' }) e1 e2
  regAllocInstr dest t cont regenv (IifLE x y e1 e2)  =
    regAllocIf dest t cont regenv (\e1' e2' -> do {
      x' <- envFind x Tint regenv;
      y' <- envFind' y regenv;
      return $ IifLE x' y' e1' e2' }) e1 e2
  regAllocInstr dest t cont regenv (IifGE x y e1 e2)  =
    regAllocIf dest t cont regenv (\e1' e2' -> do {
      x' <- envFind x Tint regenv;
      y' <- envFind' y regenv;
      return $ IifGE x' y' e1' e2' }) e1 e2
  regAllocInstr dest t cont regenv (IifFEq x y e1 e2) =
    regAllocIf dest t cont regenv (\e1' e2' -> do {
      x' <- envFind x Tfloat regenv;
      y' <- envFind y Tfloat regenv;
      return $ IifFEq x' y' e1' e2' }) e1 e2
  regAllocInstr dest t cont regenv (IifFLE x y e1 e2) =
    regAllocIf dest t cont regenv (\e1' e2' -> do {
      x' <- envFind x Tfloat regenv;
      y' <- envFind y Tfloat regenv;
      return $ IifFLE x' y' e1' e2' }) e1 e2
  regAllocInstr dest _ cont regenv (IcallCls x ys zs) =
    regAllocCall dest cont regenv (\ys' zs' -> do {
      x' <- envFind x Tint regenv;
      return $ IcallCls x' ys' zs' }) ys zs
  regAllocInstr dest _ cont regenv (IcallDir l ys zs) =
    regAllocCall dest cont regenv (\ys' zs' -> return $ IcallDir l ys' zs') ys zs
  regAllocInstr dest t cont regenv (Isave x y) =
    assert False $ regAllocInstr dest t cont regenv (Isave x y)
  regAllocInstr _    _ _    regenv i           =
    return (Ans i, regenv)

  regAllocIf :: (MonadState Counter m, MonadIO m) => String -> Type -> Seq ->
                Env -> (Seq -> Seq -> ExceptionalT (String, Type) m Instr) ->
                Seq -> Seq -> ExceptionalT (String, Type) m (Seq, Env)
  regAllocIf dest t cont regenv constr e1 e2 = do
    (e1', regenv1) <- lift $ regAllocSeq dest t cont regenv e1
    (e2', regenv2) <- lift $ regAllocSeq dest t cont regenv e2
    i              <- constr e1' e2'
    let regenv' = foldl (\env x -> if isReg x
            then env
            else
              case x `lookup` regenv1 of
                Nothing -> env
                Just r1 -> case x `lookup` regenv2 of
                  Nothing -> env
                  Just r2 ->
                    if r1 /= r2
                      then env
                      else addToAL env x r1) [] $ freeVars cont
    seq' <- foldM (\e x -> if x == dest || not (x `hasKeyAL` regenv) || x `hasKeyAL` regenv'
            then return e
            else lift $ instrSeq (Isave (fromJust $ x `lookup` regenv) x) e) (Ans i) $ freeVars cont
    return (seq', regenv')

  regAllocCall :: (MonadState Counter m, MonadIO m) => String -> Seq -> Env ->
                  ([String] -> [String] ->
                    ExceptionalT (String, Type) m Instr) -> [String] ->
                  [String] -> ExceptionalT (String, Type) m (Seq, Env)
  regAllocCall dest cont regenv constr ys zs = do
    ys'  <- mapM (\y -> envFind y Tint regenv) ys
    zs'  <- mapM (\z -> envFind z Tfloat regenv) zs
    s'   <- constr ys' zs'
    seq' <- foldM (\e x ->
      if   x == dest || not (x `hasKeyAL` regenv)
      then return e
      else lift $ instrSeq (Isave (fromJust $ x `lookup` regenv) x) e) (Ans s') $ freeVars cont
    return (seq', [])
