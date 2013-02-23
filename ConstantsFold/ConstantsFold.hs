module ConstantsFold.ConstantsFold where
  import ConstantsFold.Env

  import KNormal.KSyntax

  constantsFold :: Env -> KExpr -> KExpr
  constantsFold env (KEneg s)
    | s `memi` env                          =
      KEint $ - s `findi` env
  constantsFold env (KEadd s1 s2)
    | s1 `memi` env && s2 `memi` env        =
      KEint $ s1 `findi` env + s2 `findi` env
  constantsFold env (KEsub s1 s2)
    | s1 `memi` env && s2 `memi` env        =
      KEint $ s1 `findi` env - s2 `findi` env
  constantsFold env (KEmult s1 s2)
    | s1 `memi` env && s2 `memi` env        =
      KEint $ s1 `findi` env * s2 `findi` env
  constantsFold env (KEdiv s1 s2)
    | s1 `memi` env && s2 `memi` env        =
      KEint $ s1 `findi` env `div` s2 `findi` env
  constantsFold env (KEmod s1 s2)
    | s1 `memi` env && s2 `memi` env        =
      KEint $ s1 `findi` env `mod` s2 `findi` env
  constantsFold env (KEvar s)
    | s `memi` env                          =
      KEint $ s `findi` env
  constantsFold env (KEifEq s1 s2 e1 e2)
    | s1 `memi` env && s2 `memi` env        =
      if   s1 `findi` env == s2 `findi` env
      then constantsFold env e1
      else constantsFold env e2
  constantsFold env (KEifLE s1 s2 e1 e2)
    | s1 `memi` env && s2 `memi` env        =
      if   s1 `findi` env <= s2 `findi` env
      then constantsFold env e1
      else constantsFold env e2
  constantsFold env (KElet s e1 e2)         =
    KElet s e' $ constantsFold (extend env s e') e2
    where
      e' = constantsFold env e1
  constantsFold env (KEletRec fd e)         =
    KEletRec fd { body = constantsFold env $ body fd } $ constantsFold env e
  constantsFold env (KEletPair s1 s2 s3 e)
    | s3 `memp` env                         =
      KElet s1 (KEvar ss1) $ KElet s2 (KEvar ss2) $ constantsFold env e
      where
        (ss1, ss2) = s3 `findp` env
  constantsFold env (KEletList s1 s2 s3 e)
    | s3 `meml` env                         =
      KElet s1 (KEvar ss1) $ KElet s2 (KEvar ss2) $ constantsFold env e
      where
        (ss1, ss2) = s3 `findl` env
  constantsFold env (KEhandle e1 e2)        =
    KEhandle (constantsFold env e1) $ constantsFold env e2
  constantsFold env (KEseq e1 e2)           =
    KEseq (constantsFold env e1) $ constantsFold env e2
  constantsFold _   e                       =
    e