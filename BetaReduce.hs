module BetaReduce (betaReduce) where
  import qualified BetaReduce.BetaReduce as BR
  import KNormal.KSyntax
  import AlphaConvert.Env

  betaReduce :: KExpr -> IO KExpr
  betaReduce = BR.betaReduce emptyEnv
