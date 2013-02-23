module LetFlatten (letFlatten) where
  import KNormal.KSyntax

  letFlatten :: KExpr -> KExpr
  letFlatten (KEifEq s1 s2 e1 e2)   =
    KEifEq s1 s2 (letFlatten e1) $ letFlatten e2
  letFlatten (KEifLE s1 s2 e1 e2)   =
    KEifLE s1 s2 (letFlatten e1) $ letFlatten e2
  letFlatten (KElet s e1 e2)        =
    insert (letFlatten e1) where
      insert :: KExpr -> KExpr
      insert (KElet y e3 e4)        = KElet y e3 $ insert e4
      insert (KEletRec fd e)        = KEletRec fd $ insert e
      insert (KEletPair s1 s2 s3 e) = KEletPair s1 s2 s3 $ insert e
      insert (KEletList s1 s2 s3 e) = KEletList s1 s2 s3 $ insert e
      insert e                      = KElet s e $ letFlatten e2
  letFlatten (KEletRec fd e)        =
    KEletRec fd { body = letFlatten . body $ fd } $ letFlatten e
  letFlatten (KEletPair s1 s2 s3 e) =
    KEletPair s1 s2 s3 $ letFlatten e
  letFlatten (KEletList s1 s2 s3 e) =
    KEletList s1 s2 s3 $ letFlatten e
  letFlatten (KEhandle e1 e2)       =
    KEhandle (letFlatten e1) $ letFlatten e2
  letFlatten (KEseq e1 e2)          =
    KEseq (letFlatten e1) $ letFlatten e2
  letFlatten e                      =
    e
