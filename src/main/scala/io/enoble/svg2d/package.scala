package io
package enoble

import cats.Monoid
import io.enoble.svg2d.ast.FastMonoid

package object svg2d {
  type Coords = (Double, Double)
  type SBAction = StringBuilder => Unit
  type RWRSBAction = Vector[SBAction]

  implicit def sbActionMonoid: FastMonoid[String, SBAction] = new FastMonoid[String, SBAction] {
    override implicit val monoid: Monoid[SBAction] = new Monoid[SBAction] {
      override def empty: SBAction = _ => ()

      override def combine(f1: SBAction, f2: SBAction): SBAction = { sb => f1(sb); f2(sb) }
    }

    override def in(str: String): SBAction =
      sb => sb ++= str
  }

  implicit def rwrsbActionMonoid: FastMonoid[String, RWRSBAction] = new FastMonoid[String, RWRSBAction] {
    override implicit val monoid: Monoid[RWRSBAction] =
      cats.instances.vector.catsKernelStdMonoidForVector[SBAction]

    override def in(str: String): RWRSBAction = Vector.empty :+ { (sb: StringBuilder) => sb ++= str; () }
  }

  implicit final class rwrSBActionOps(val action: RWRSBAction) extends AnyVal {
    def asString: String = {
      val sb = new StringBuilder()
      action.foreach(f => f(sb))
      sb.result()
    }
  }
}
