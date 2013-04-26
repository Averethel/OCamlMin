module SPARC.Utils where
  import SPARC.Syntax

  concat :: Seq -> String -> Seq -> Seq
  concat (Ans e1)     x e2 = Let x e1 e2
  concat (Let y e e1) x e2 = Let y e $ SPARC.Utils.concat e1 x e2

  align :: Integer -> Integer
  align i
    | i `mod` 8 == 0 = i
    | otherwise    = i + 4
