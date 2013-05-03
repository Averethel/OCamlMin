module RegAlloc.Alloc where
  import RegAlloc.Env
  import RegAlloc.Targetting
  import SPARC.Syntax
  import SPARC.Utils
  import Types

  import Control.Exception.Base
  import Data.List hiding (insert)
  import Data.List.Utils
  import Data.Maybe
  import Data.Set hiding (foldl, map)

  data AllocResult =
      Alloc String -- allocated register
    | Spill String -- spilled variable


  alloc :: String -> Type -> Seq -> Env -> String -> Type -> IO AllocResult
  alloc _    _ _    regenv x Tunit = assert (not $ x `hasKeyAL` regenv) $ return $ Alloc "%g0"
  alloc dest t cont regenv x tp
    | isReg x                      = assert (not $ x `hasKeyAL` regenv) $ return $ Alloc x
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
          putStrLn $ "Allocated " ++ show x ++ " to " ++ show r ++ "."
          return $ Alloc reg
        Nothing  -> do
          putStrLn $ "Register allocation failed for " ++ show x ++ "."
          let Just y = find (\e -> not (isReg y) && (case e `lookup` regenv of {
            Nothing -> False;
            Just z  -> z `elem` allRegs})) $ reverse fvs
          putStrLn $ "spilling " ++ show y ++ " from " ++ show (fromJust $ y `lookup` regenv) ++ "."
          return $ Spill y

  regAllocSeq :: String -> Type -> Seq -> Env -> Seq -> IO (Seq, Env)
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
          return (Seq (Ans save) $ SPARC.Utils.concat s1 r tx s2, regenv'')
        Alloc r -> do
          (s2, regenv'') <- regAllocSeq dest t cont (addToAL regenv' x r) s
          return (SPARC.Utils.concat s1 r tx s2, regenv'')
  regAllocSeq dest t cont regenv (Seq s1 s2)    = do
    let cont'        = SPARC.Utils.concat s2 dest t cont
    -- not sure if %g0 is correct dest
    (s1', regenv')  <- regAllocSeq dest  t     cont' regenv  s1
    (s2', regenv'') <- regAllocSeq "%g0" Tunit cont  regenv' s2
    return (Seq s1' s2', regenv'')
  regAllocSeq dest t cont regenv (Labeled l s)  = do
    (s', regenv') <- regAllocSeq dest t cont regenv s
    return (Labeled l s', regenv')

  regAllocInstrAndRestore :: String -> Type -> Seq -> Env -> Instr -> IO (Seq, Env)
  regAllocInstrAndRestore dest t cont regenv i =  do
    instrAlloc <- regAllocInstr dest t cont regenv i
    case instrAlloc of
      Left (x, tx) -> do
        putStrLn $ "Restoring " ++ show x ++ "."
        regAllocSeq dest t cont regenv $ Let x tx (Irestore x) $ Ans i
      Right r      -> return r

  regAllocSingleArg :: Type -> (String -> Instr) -> String -> Env -> IO (Either (String, Type) (Seq, Env))
  regAllocSingleArg rType constr x regenv =
    return $ case envFind x rType regenv of
      Left err -> Left err
      Right s  -> Right (Ans $ constr s, regenv)

  regAllocDoubleArgInt :: (String -> IdOrIimm -> Instr) -> String -> IdOrIimm -> Env -> IO (Either (String, Type) (Seq, Env))
  regAllocDoubleArgInt constr x y regenv =
    return $ case envFind x Tint regenv of
      Left err -> Left err
      Right x' -> case envFind' y regenv of
        Left err -> Left err
        Right y' -> Right (Ans $ constr x' y', regenv)

  regAllocDoubleArgFloat :: (String -> String -> Instr) -> String -> String -> Env -> IO (Either (String, Type) (Seq, Env))
  regAllocDoubleArgFloat constr x y regenv =
    return $ case envFind x Tfloat regenv of
      Left err -> Left err
      Right x' -> case envFind y Tfloat regenv of
        Left err -> Left err
        Right y' -> Right (Ans $ constr x' y', regenv)

  regAllocTripleArg :: Type -> Type -> (String -> String -> IdOrIimm -> Instr) -> String -> String -> IdOrIimm -> Env -> IO (Either (String, Type) (Seq, Env))
  regAllocTripleArg xType yType constr x y z regenv =
    return $ case envFind x xType regenv of
      Left err -> Left err
      Right x' -> case envFind y yType regenv of
        Left err -> Left err
        Right y' -> case envFind' z regenv of
          Left err -> Left err
          Right z' -> Right (Ans $ constr x' y' z', regenv)

  regAllocInstr :: String -> Type -> Seq -> Env -> Instr -> IO (Either (String, Type) (Seq, Env))
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
  regAllocInstr _    _ _    regenv (IfModD x y)         =
    regAllocDoubleArgFloat IfModD x y regenv
  regAllocInstr _    _ _    regenv (IldDF x y)          =
    regAllocDoubleArgInt IldDF x y regenv
  regAllocInstr _    _ _    regenv (IstDF x y z)        =
    regAllocTripleArg Tfloat Tint IstDF x y z regenv
  regAllocInstr dest t cont regenv (IifEq x y e1 e2)  =
    regAllocIf dest t cont regenv (\e1' e2' ->
      case envFind x Tint regenv of
        Left err -> Left err
        Right x' -> case envFind' y regenv of
          Left err -> Left err
          Right y' -> Right $ IifEq x' y' e1' e2') e1 e2
  regAllocInstr dest t cont regenv (IifLE x y e1 e2)  =
    regAllocIf dest t cont regenv (\e1' e2' ->
      case envFind x Tint regenv of
        Left err -> Left err
        Right x' -> case envFind' y regenv of
          Left err -> Left err
          Right y' -> Right $ IifLE x' y' e1' e2') e1 e2
  regAllocInstr dest t cont regenv (IifGE x y e1 e2)  =
    regAllocIf dest t cont regenv (\e1' e2' ->
      case envFind x Tint regenv of
        Left err -> Left err
        Right x' -> case envFind' y regenv of
          Left err -> Left err
          Right y' -> Right $ IifGE x' y' e1' e2') e1 e2
  regAllocInstr dest t cont regenv (IifFEq x y e1 e2) =
    regAllocIf dest t cont regenv (\e1' e2' ->
      case envFind x Tfloat regenv of
        Left err -> Left err
        Right x' -> case envFind y Tfloat regenv of
          Left err -> Left err
          Right y' -> Right $ IifFEq x' y' e1' e2') e1 e2
  regAllocInstr dest t cont regenv (IifFLE x y e1 e2) =
    regAllocIf dest t cont regenv (\e1' e2' ->
      case envFind x Tfloat regenv of
        Left err -> Left err
        Right x' -> case envFind y Tfloat regenv of
          Left err -> Left err
          Right y' -> Right $ IifFLE x' y' e1' e2') e1 e2
  regAllocInstr dest _ cont regenv (IcallCls x ys zs) =
    regAllocCall dest cont regenv (\ys' zs' ->
      case envFind x Tint regenv of
        Left err -> Left err
        Right x' -> Right $ IcallCls x' ys' zs') ys zs
  regAllocInstr dest _ cont regenv (IcallDir l ys zs) =
    regAllocCall dest cont regenv (\ys' zs' -> Right $ IcallDir l ys' zs') ys zs
  regAllocInstr dest t cont regenv (Isave x y) =
    assert False $ regAllocInstr dest t cont regenv (Isave x y)
  regAllocInstr _    _ _    regenv i           =
    return $ Right (Ans i, regenv)

  regAllocIf :: String -> Type -> Seq -> Env -> (Seq -> Seq -> Either (String, Type) Instr) -> Seq -> Seq -> IO (Either (String, Type) (Seq, Env))
  regAllocIf dest t cont regenv constr e1 e2 = do
    (e1', regenv1) <- regAllocSeq dest t cont regenv e1
    (e2', regenv2) <- regAllocSeq dest t cont regenv e2
    case constr e1' e2' of
      Left err -> return $ Left err
      Right i  -> do
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
        let seq' = foldl (\e x -> if x == dest || not (x `hasKeyAL` regenv) || x `hasKeyAL` regenv'
            then e
            else Seq (Ans $ Isave (fromJust $ x `lookup` regenv) x) e) (Ans i) $ freeVars cont
        return $ Right (seq', regenv')

  regAllocCall :: String -> Seq -> Env -> ([String] -> [String] -> Either (String, Type) Instr) -> [String] -> [String] -> IO (Either (String, Type) (Seq, Env))
  regAllocCall dest cont regenv constr ys zs = do
    let Right s' = constr
                    (map (\y -> let Right y' = envFind y Tint regenv in y') ys)
                    (map (\z -> let Right z' = envFind z Tfloat regenv in z') zs)
    let seq' = Ans s'
    return $ Right (foldl (\e x ->
      if   x == dest || not (x `hasKeyAL` regenv)
      then e
      else Seq (Ans $ Isave (fromJust $ x `lookup` regenv) x) e) seq' $ freeVars cont, [])