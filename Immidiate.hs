module Immidiate (optimizeProgram) where
  import SPARC.Syntax
  import SPARC.Utils

  import Data.Bits
  import Data.Maybe

  type Env = [(String, Integer)]

  member :: String -> Env -> Bool
  member _ []           = False
  member s ((x, _):xs)
    | x == s            = True
    | otherwise         = s `member` xs

  find :: String -> Env -> Integer
  find s env = fromJust $ s `lookup` env

  optimizeSeq :: Env -> Seq -> IO Seq
  optimizeSeq env (Ans i)    = do
    i' <- optimizeInstr env i
    return $ Ans i'
  optimizeSeq env (Let x t (Iset n) e)
    | -4096 <= n && n < 4096 = do
      putStrLn $ "found simm13 " ++ show x ++ " = " ++ show n ++ "."
      e' <- optimizeSeq ((x, n):env) e
      if x `elem` freeVars e'
      then return $ Let x t (Iset n) e'
      else do
        putStrLn $ "erased redundant Set to " ++ show x
        return e'
  optimizeSeq env (Let x t (ISLL y (C i)) e)
    | y `member` env         = do
      putStrLn $ "erased redundant SLL on " ++ show x
      optimizeSeq env $
        Let x t (Iset $ y `find` env `shift` fromInteger i) e
  optimizeSeq env (Let x t i e) = do
    e' <- optimizeSeq env e
    i' <- optimizeInstr env i
    return $ Let x t i' e'
  optimizeSeq env (Seq e1 e2) = do
    e1' <- optimizeSeq env e1
    e2' <- optimizeSeq env e2
    return $ Seq e1' e2'
  optimizeSeq env (Labeled l e) = do
    e' <- optimizeSeq env e
    return $ Labeled l e'

  optimizeInstr :: Env -> Instr -> IO Instr
  optimizeInstr env (Iadd x (V y))
    | y `member` env                   =
      return $ Iadd x  $ C $ y `find` env
  optimizeInstr env (Isub x (V y))
    | y `member` env                   =
      return $ Isub x $ C $ y `find` env
  optimizeInstr env (ISLL x (V y))
    | y `member` env                   =
      return $ ISLL x $ C $ y `find` env
  optimizeInstr env (Ild x (V y))
    | y `member` env                   =
      return $ Ild x $ C $ y `find` env
  optimizeInstr env (Ist x y (V z))
    | z `member` env                   =
      return $ Ist x y $ C $ z `find` env
  optimizeInstr env (IldDF x (V y))
    | y `member` env                   =
      return $ IldDF x $ C $ y `find` env
  optimizeInstr env (IstDF x y (V z))
    | z `member` env                   =
      return $ IstDF x y $ C $ z `find` env
  optimizeInstr env (IifEq x (V y) e1 e2)
    | y `member` env                   = do
      e1' <- optimizeSeq env e1
      e2' <- optimizeSeq env e2
      return $ IifEq x (C $ y `find` env) e1' e2'
  optimizeInstr env (IifLE x (V y) e1 e2)
    | y `member` env                   = do
      e1' <- optimizeSeq env e1
      e2' <- optimizeSeq env e2
      return $ IifLE x (C $ y `find` env) e1' e2'
  optimizeInstr env (IifGE x (V y) e1 e2)
    | y `member` env                   = do
      e1' <- optimizeSeq env e1
      e2' <- optimizeSeq env e2
      return $ IifGE x (C $ y `find` env) e1' e2'
  optimizeInstr env (IifEq x (V y) e1 e2)
    | x `member` env                   = do
      e1' <- optimizeSeq env e1
      e2' <- optimizeSeq env e2
      return $ IifEq y (C $ x `find` env) e1' e2'
  optimizeInstr env (IifLE x (V y) e1 e2)
    | x `member` env                   = do
      e1' <- optimizeSeq env e1
      e2' <- optimizeSeq env e2
      return $ IifGE y (C $ x `find` env) e1' e2'
  optimizeInstr env (IifGE x (V y) e1 e2)
    | x `member` env                   = do
      e1' <- optimizeSeq env e1
      e2' <- optimizeSeq env e2
      return $ IifLE y (C $ x `find` env) e1' e2'
  optimizeInstr env (IifEq x y' e1 e2) = do
    e1' <- optimizeSeq env e1
    e2' <- optimizeSeq env e2
    return $ IifEq x y' e1' e2'
  optimizeInstr env (IifLE x y' e1 e2) = do
    e1' <- optimizeSeq env e1
    e2' <- optimizeSeq env e2
    return $ IifLE x y' e1' e2'
  optimizeInstr env (IifGE x y' e1 e2) = do
    e1' <- optimizeSeq env e1
    e2' <- optimizeSeq env e2
    return $ IifGE x y' e1' e2'
  optimizeInstr env (IifFEq x y e1 e2) = do
    e1' <- optimizeSeq env e1
    e2' <- optimizeSeq env e2
    return $ IifFEq x y e1' e2'
  optimizeInstr env (IifFLE x y e1 e2) = do
    e1' <- optimizeSeq env e1
    e2' <- optimizeSeq env e2
    return $ IifFLE x y e1' e2'
  optimizeInstr _   e                  =
    return e

  optmizeFd :: FunDef -> IO FunDef
  optmizeFd fd = do
    b' <- optimizeSeq [] $ body fd
    return fd{ body = b' }

  optimizeProgram :: Program -> IO Program
  optimizeProgram p = do
    tp' <- mapM optmizeFd $ toplevel p
    m'  <- optimizeSeq [] $ main p
    return p{ toplevel = tp', main = m' }
