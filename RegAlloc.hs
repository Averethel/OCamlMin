module RegAlloc (regAllocProgram) where
  import SPARC.Syntax
  import SPARC.Utils
  import Types

  import RegAlloc.Alloc

  import Control.Exception.Base
  import Data.List.Utils

  regAllocFunDef :: FunDef -> IO FunDef
  regAllocFunDef fd = do
    let L x = name fd
    let initEnv = [(x, regCl)]
    let (_, argRegs, env) = foldl (\(i, argRegs', env') y ->
          let r = regs !! i in
          (i + 1, argRegs' ++ [r], assert (not $ isReg y) $ addToAL env' y r))
          (0, [], initEnv) $ args fd
    let (_, fArgRegs, env') = foldl (\(d, fArgRegs', env'') z ->
          let fr = fregs !! d in
          (d + 1, fArgRegs' ++ [fr], assert (not $ isReg z) $ addToAL env'' z fr))
          (0, [], env) $ fargs fd
    let a = case ret fd of
          Tunit  -> "%g0"
          Tfloat -> head fregs
          _      -> head regs
    (e', _) <- regAllocSeq a (ret fd) (Ans $ Imov a) env' $ body fd
    return $ fd{ args = argRegs, fargs = fArgRegs, body = e' }

  regAllocProgram :: Program -> IO Program
  regAllocProgram p = do
    defs'      <- mapM regAllocFunDef $ toplevel p
    (main', _) <- regAllocSeq "%g0" Tunit (Ans Inop) [] $ main p
    return $ p{ toplevel = defs', main = main' }
