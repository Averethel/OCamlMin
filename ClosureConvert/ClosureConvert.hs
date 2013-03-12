module ClosureConvert.ClosureConvert where
  import ClosureConvert.CSyntax

  import KNormal.KSyntax

  import Data.Set

  closureConvert :: [Label] -> KExpr -> IO Program
  closureConvert _    KEunit               =
    return $ P [] CEunit
  closureConvert _    KEnil                =
    return $ P [] CEnil
  closureConvert _    (KEint n)            =
    return $ P [] $ CEint n
  closureConvert _    (KEneg x)            =
    return $ P [] $ CEneg x
  closureConvert _    (KEload r)           =
    return $ P [] $ CEload r
  closureConvert _    (KEadd x y)          =
    return $ P [] $ CEadd x y
  closureConvert _    (KEsub x y)          =
    return $ P [] $ CEsub x y
  closureConvert _    (KEmult x y)         =
    return $ P [] $ CEmult x y
  closureConvert _    (KEdiv x y)          =
    return $ P [] $ CEdiv x y
  closureConvert _    (KEmod x y)          =
    return $ P [] $ CEmod x y
  closureConvert _    (KEstore r x)        =
    return $ P [] $ CEstore r x
  closureConvert _    (KEvar x)            =
    return $ P [] $ CEvar x
  closureConvert _    (KEerror m)          =
    return $ P [] $ CEerror m
  closureConvert known (KEifEq x y e1 e2)   = do
    p1 <- closureConvert known e1
    p2 <- closureConvert known e2
    return $ P (definitions p1 ++ definitions p2)
           $ CEifEq x y (main p1) $ main p2
  closureConvert known (KEifLE x y e1 e2)   = do
    p1 <- closureConvert known e1
    p2 <- closureConvert known e2
    return $ P (definitions p1 ++ definitions p2)
           $ CEifLE x y (main p1) $ main p2
  closureConvert known (KElet x e1 e2)      = do
    p1 <- closureConvert known e1
    p2 <- closureConvert known e2
    return $ P (definitions p1 ++ definitions p2)
           $ CElet x (main p1) $ main p2
  closureConvert known (KEletRec fd e)      = do
    let x  = KNormal.KSyntax.name fd
    let as = KNormal.KSyntax.args fd
    let e1 = KNormal.KSyntax.body fd
    let known' = L x : known
    p1 <- closureConvert known' e1
    let zs = ClosureConvert.CSyntax.freeVars (main p1) \\ fromList as
    (p1', known'') <-
      if zs /= empty
      then do
        putStrLn $ "Free variable(s) " ++ show (toList zs) ++
                   " found in function " ++ x ++ "."
        putStrLn $ "Function " ++ x ++ " cannot be applied directly."
        p <- closureConvert known e1
        return (p, known)
      else
        return (p1, known')
    p2 <- closureConvert known'' e
    let fvs = toList $ ClosureConvert.CSyntax.freeVars (main p1') \\
                       singleton x `union` fromList as
    let df = ClosureConvert.CSyntax.FD { ClosureConvert.CSyntax.name = L x,
             ClosureConvert.CSyntax.args = as,
                  formalFvs = fvs, ClosureConvert.CSyntax.body = main p1' }
    let dfs = df : (definitions p1' ++ definitions p2)
    if x `member` ClosureConvert.CSyntax.freeVars (main p2)
    then
      return $ P dfs $ CEmakeClj x C { entry = L x, actFvs = fvs } $ main p2
    else do
      putStrLn $ "Eliminating closure " ++ x
      return $ P dfs $ main p2
  closureConvert known (KEapply s ss)
    | L s `elem` known                      = do
      putStrLn $ "Directly applying " ++ s
      return $ P [] $ CEappDir (L s) ss
    | otherwise                             =
      return $ P [] $ CEappClj s ss
  closureConvert _     (KEpair a b)         =
    return $ P [] $ CEpair a b
  closureConvert _     (KEcons h t)         =
    return $ P [] $ CEcons h t
  closureConvert known (KEletPair a b p e)  = do
    pr <- closureConvert known e
    return $ P (definitions pr) $ CEletPair a b p $ main pr
  closureConvert known (KEletList h t l e)  = do
    p <- closureConvert known e
    return $ P (definitions p) $ CEletList h t l $ main p
  closureConvert known (KEhandle e1 e2)     = do
    p1 <- closureConvert known e1
    p2 <- closureConvert known e2
    return $ P (definitions p1 ++ definitions p2) $ CEhandle (main p1) $ main p2
  closureConvert known (KEseq e1 e2)        = do
    p1 <- closureConvert known e1
    p2 <- closureConvert known e2
    return $ P (definitions p1 ++ definitions p2) $ CEseq (main p1) $ main p2
  closureConvert _    (KEextFunApp s ss)   =
    return $ P [] $ CEappDir (L s) ss