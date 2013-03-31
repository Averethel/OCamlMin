module TypedSyntax.Constructor where
  import Syntax.Constructor
  import Types

  import Utils.Iseq

  import Control.Exception.Base

  type TypedConstructor = (Constructor, Type)

  constructors :: TypedConstructor -> [TypedConstructor]
  constructors (CNnil, tp@(Tlist t))        =
    [(CNnil, tp), (CNcons, Tfun [t, tp] tp)]
  constructors (CNcons, tp@(Tfun [_, _] t)) =
    [(CNnil, t), (CNcons, tp)]
  constructors (CNpair, t)                  =
    [(CNpair, t)]
  constructors (CNtrue, t)                  =
    [(CNtrue, t), (CNfalse, t)]
  constructors (CNfalse, t)                 =
    [(CNtrue, t), (CNfalse, t)]
  constructors (CNunit, t)                  =
    [(CNunit, t)]
  constructors c                            =
    assert False $ constructors c

  pprTypedConstructor :: TypedConstructor -> Iseq
  pprTypedConstructor (c, t) = iConcat [ pprConstructor c, iStr " : ",
                                         pprType t ]
