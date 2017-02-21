package io
package enoble

import scalaz.Monoid

package object svg2d {
  type Coords = (Double, Double)
  type SBAction = StringBuilder => Unit
  type RWRSBAction = Vector[SBAction]

  implicit def sbActionMonoid: Monoid[SBAction] = new Monoid[SBAction] {
    override def zero: SBAction = _ => ()

    override def append(f1: SBAction, f2: => SBAction): SBAction = { sb => f1(sb); f2(sb) }
  }

  implicit def rwrsbActionMonoid: Monoid[RWRSBAction] = new Monoid[RWRSBAction] {
    override def zero: RWRSBAction = Vector.empty

    override def append(f1: RWRSBAction, f2: => RWRSBAction): RWRSBAction = f1 ++ f2
  }
}
