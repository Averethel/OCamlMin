module AlphaConvert.Env where
  import Data.Maybe

  type Env = [(String, String)]

  emptyEnv :: Env
  emptyEnv = []

  find :: Env -> String -> String
  find e x = fromMaybe x $ x `lookup` e

  extend :: Env -> String -> String -> Env
  extend env x s = (x, s) : env

  addList :: Env -> [(String, String)] -> Env
  addList = flip (++)
