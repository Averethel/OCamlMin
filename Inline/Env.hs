module Inline.Env where
  import KNormal.KSyntax

  import qualified Data.List as L
  import Data.Maybe

  type Env = [FunDef]

  emptyEnv :: Env
  emptyEnv = []

  member :: String -> Env -> Bool
  member n = any (\x -> (fst . name $ x) == n)

  find :: Env -> String -> ([String], KExpr)
  find e n =
    let fd = fromJust $ L.find (\x -> (fst . name $ x) == n) e
    in (map fst $ args fd, body fd)

  extend :: Env -> FunDef -> Env
  extend = flip (:)
