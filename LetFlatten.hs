module LetFlatten (letFlatten) where
  import KNormal.KSyntax

  letFlatten :: KExpr -> KExpr
  letFlatten (KEifEq s1 s2 e1 e2 t)   =
    KEifEq s1 s2 (letFlatten e1) (letFlatten e2) t
  letFlatten (KEifLE s1 s2 e1 e2 t)   =
    KEifLE s1 s2 (letFlatten e1) (letFlatten e2) t
  letFlatten (KElet s e1 e2 t)        =
    insert (letFlatten e1) where
      insert :: KExpr -> KExpr
      insert (KElet y e3 e4 t')        = KElet y e3 (insert e4) t'
      insert (KEletRec fd e t')        = KEletRec fd (insert e) t'
      insert (KEletPair s1 s2 s3 e t') = KEletPair s1 s2 s3 (insert e) t'
      insert (KEletList s1 s2 s3 e t') = KEletList s1 s2 s3 (insert e) t'
      insert e                         = KElet s e (letFlatten e2) t
  letFlatten (KEletRec fd e t)        =
    KEletRec fd { body = letFlatten . body $ fd } (letFlatten e) t
  letFlatten (KEletPair s1 s2 s3 e t) =
    KEletPair s1 s2 s3 (letFlatten e) t
  letFlatten (KEletList s1 s2 s3 e t) =
    KEletList s1 s2 s3 (letFlatten e) t
  letFlatten (KEhandle e1 e2 t)       =
    KEhandle (letFlatten e1) (letFlatten e2) t
  letFlatten (KEseq e1 e2 t)          =
    KEseq (letFlatten e1) (letFlatten e2) t
  letFlatten e                        =
    e
