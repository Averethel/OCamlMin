module ConstantsFold.ConstantsFold where
  import ConstantsFold.Env

  import KNormal.KSyntax
  import Types

  constantsFold :: Env -> KExpr -> KExpr
  constantsFold env (KEneg (s, _) _)
    | s `memi` env                          =
      KEint (- s `findi` env) Tint
  constantsFold env (KEadd (s1, _) (s2, _) _)
    | s1 `memi` env && s2 `memi` env        =
      KEint (s1 `findi` env + s2 `findi` env) Tint
  constantsFold env (KEsub (s1, _) (s2, _) _)
    | s1 `memi` env && s2 `memi` env        =
      KEint (s1 `findi` env - s2 `findi` env) Tint
  constantsFold env (KEmult (s1, _) (s2, _) _)
    | s1 `memi` env && s2 `memi` env        =
      KEint (s1 `findi` env * s2 `findi` env) Tint
  constantsFold env (KEdiv (s1, _) (s2, _) _)
    | s1 `memi` env && s2 `memi` env        =
      KEint (s1 `findi` env `div` s2 `findi` env) Tint
  constantsFold env (KEvar s _)
    | s `memi` env                          =
      KEint (s `findi` env) Tint
  constantsFold env (KEifEq (s1, _) (s2, _) e1 e2 _)
    | s1 `memi` env && s2 `memi` env        =
      if   s1 `findi` env == s2 `findi` env
      then constantsFold env e1
      else constantsFold env e2
  constantsFold env (KEifLE (s1, _) (s2, _) e1 e2 _)
    | s1 `memi` env && s2 `memi` env        =
      if   s1 `findi` env <= s2 `findi` env
      then constantsFold env e1
      else constantsFold env e2
  constantsFold env (KElet s e1 e2 t)       =
    KElet s e' (constantsFold (extend env (fst s) e') e2) t
    where
      e' = constantsFold env e1
  constantsFold env (KEletRec fd e t)       =
    KEletRec fd { body = constantsFold env $ body fd } (constantsFold env e) t
  constantsFold env (KEletPair s1 s2 (s3, _) e t)
    | s3 `memp` env                         =
      KElet s1 (KEvar ss1 tt1)
               (KElet s2 (KEvar ss2 tt2) (constantsFold env e) t) t
      where
        ((ss1, tt1), (ss2, tt2)) = s3 `findp` env
  constantsFold env (KEletList s1 s2 (s3, _) e t)
    | s3 `meml` env                         =
      KElet s1 (KEvar ss1 tt1)
               (KElet s2 (KEvar ss2 tt2) (constantsFold env e) t) t
      where
        ((ss1, tt1), (ss2, tt2)) = s3 `findl` env
  constantsFold env (KEhandle e1 e2 t)      =
    KEhandle (constantsFold env e1) (constantsFold env e2) t
  constantsFold env (KEseq e1 e2 t)         =
    KEseq (constantsFold env e1) (constantsFold env e2) t
  constantsFold _   e                       =
    e
