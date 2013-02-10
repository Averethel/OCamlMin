{-# LANGUAGE
  FlexibleContexts
  #-}

module TypeInference.Constructor where
  import Syntax.Constructor
  import Types

  import TypeInference.Counter
  import TypeInference.Env

  import Control.Exception.Base
  import Control.Monad.Error
  import Control.Monad.State

  typeAndBindingsOfConstructor :: (MonadState Counter m, MonadError String m) =>
                                  Constructor -> [String] -> m (Type, Env)
  typeAndBindingsOfConstructor CNnil   []     = do
    v <- freshVar
    return (Tlist v, [])
  typeAndBindingsOfConstructor CNcons  [h, t] = do
    v <- freshVar
    return (Tlist v, [(h, v), (t, Tlist v)])
  typeAndBindingsOfConstructor CNpair  [x, y] = do
    v1 <- freshVar
    v2 <- freshVar
    return (Tpair v1 v2, [(x, v1), (y, v2)])
  typeAndBindingsOfConstructor CNtrue  []     =
    return (Tbool, [])
  typeAndBindingsOfConstructor CNfalse []     =
    return (Tbool, [])
  typeAndBindingsOfConstructor CNunit  []     =
    return (Tunit, [])
  typeAndBindingsOfConstructor c       as     =
    assert (arity c == length as) typeAndBindingsOfConstructor c as
