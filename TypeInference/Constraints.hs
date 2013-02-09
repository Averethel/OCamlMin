module TypeInference.Constraints where
  import Types

  type Constraints = [(Type, Type)]

  emptyConstraints :: Constraints
  emptyConstraints = []

  singleConstraint :: Type -> Type -> Constraints
  singleConstraint t1 t2 = [(t1, t2)]

  addConstraints :: Constraints -> Constraints -> Constraints
  addConstraints = (++)
