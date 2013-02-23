module Compiler (compile) where
  import AlphaConvert
  import BetaReduce
  import ConstantsFold
  import Inline
  import KNormal
  import LetFlatten
  import PatternMatching
  import Syntax
  import TypeInference

  compiler :: Integer -> Expr -> IO KExpr
  compiler t e0 = do
    let e1 = compilePatternMatching e0
    let e2 = convertToKNormal e1
    let e3 = alphaConvert e2
    e4 <- betaReduce e3
    let e5 = letFlatten e4
    e6 <- inline t e5
    let e7 = constantsFold e6
    return e7

  compile :: Integer -> Expr -> IO (Either String KExpr)
  compile inlineTreshold expr = case typeOfExpression emptyEnv expr of
    Left er -> return $ Left er
    Right _ -> do
      c <- compiler inlineTreshold expr
      return $ Right c
