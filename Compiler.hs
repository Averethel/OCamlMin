module Compiler (compile) where
  import KNormal
  import PatternMatching
  import Syntax
  import TypeInference

  compile :: Expr -> Either String KExpr
  compile expr = case typeOfExpression emptyEnv expr of
    Left er -> Left er
    Right _ -> Right . convertToKNormal . compilePatternMatching $ expr
