package mylib.deep

import ch.epfl.data.sc.pardis.deep.scalalib.{Tuple3ExtOps, Tuple3Ops}
import ch.epfl.data.sc.pardis.deep.{DSLExtOpsClass, DSLExpOpsClass}
import mylib.compiler.Optim

class MyLibDSL extends DSLExpOpsClass with Tuple3Ops with ListOps with Optim.Online

class MyLibDSLExt extends DSLExtOpsClass with Tuple3ExtOps with ListExtOps

