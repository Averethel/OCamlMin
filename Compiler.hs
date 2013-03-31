module Compiler (compile) where
  import AlphaConvert
  import BetaReduce
  import ClosureConvert
  import ConstantsFold
  import EliminateDefinitions
  import Inline
  import KNormal
  import LetFlatten
  import PatternMatching
  import Syntax
  import TypedSyntax
  import TypeInference

  compiler :: Integer -> TypedExpr -> IO Program
  compiler t e0 = do
    let e1 = compilePatternMatching e0
    let e2 = convertToKNormal e1
    let e3 = alphaConvert e2
    e4 <- betaReduce e3
    let e5 = letFlatten e4
    e6 <- inline t e5
    let e7 = constantsFold e6
    e8 <- eliminateDefinitions e7
    closureConvert e8

  compile :: Integer -> Expr -> IO (Either String Program)
  compile inlineTreshold expr = case typeOfExpression emptyEnv expr of
    Left er -> return $ Left er
    Right t -> do
      c <- compiler inlineTreshold t
      return $ Right c
