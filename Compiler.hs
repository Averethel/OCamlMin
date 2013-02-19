module Compiler (compile) where
  import AlphaConvert
  import BetaReduce
  import KNormal
  import PatternMatching
  import Syntax
  import TypeInference

  compiler :: Expr -> IO KExpr
  compiler e0 = do
    let e1 = compilePatternMatching e0
    let e2 = convertToKNormal e1
    let e3 = alphaConvert e2
    betaReduce e3

  compile :: Expr -> IO (Either String KExpr)
  compile expr = case typeOfExpression emptyEnv expr of
    Left er -> return $ Left er
    Right _ -> do
      c <- compiler expr
      return $ Right c
