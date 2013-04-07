module Utils.Errors (
  unboundVariable,
  overlappingIds,
  cannotUnify,
  matchFailure,
  equalitySimple
) where

  unboundVariable :: String -> String
  unboundVariable v = "Unbound variable " ++ v

  overlappingIds :: Show a => a -> String
  overlappingIds p = "Overlapping identifires in: " ++ show p

  cannotUnify :: Show a => a -> a -> String
  cannotUnify t1 t2 = "Cannot unify " ++ show t1 ++ " with " ++ show t2

  matchFailure :: String
  matchFailure = "Match failure"

  equalitySimple :: Show a => a -> String
  equalitySimple tp = "Equality is only allowed for simple types. " ++ show tp ++ " is not a simple type."
