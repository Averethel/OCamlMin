module RegAlloc.Env where
  import X86.Syntax
  import X86.Utils
  import Types

  import Control.Monad
  import Control.Monad.Exception.Synchronous

  type Env = [(String, String)]

  envFind :: Monad m => String -> Type -> Env ->
              ExceptionalT (String, Type) m String
  envFind x t regenv
    | isReg x   = return x
    | otherwise =
      case x `lookup` regenv of
        Nothing -> throwT (x, t)
        Just y  -> return y

  envFind' :: Monad m => IdOrImm -> Env ->
                ExceptionalT (String, Type) m IdOrImm
  envFind' (V x) regenv = liftM V $ envFind x Tint regenv
  envFind' c     _      = return c
