package io
package enoble
package svg2d
package ast

import cats.Monoid

trait FastMonoid[S, A] {
  implicit val monoid: Monoid[A]
  // intended to be a monoid homomorphism, up to observation.
  def in(str: S): A
}

object FastMonoid {
  implicit def underlyingMonoid[S, A](implicit sm: FastMonoid[S, A]): Monoid[A] =
    sm.monoid

  final case class Id[A]()(implicit override val monoid: Monoid[A]) extends FastMonoid[A, A] {
    override def in(a: A): A = a
  }

  final case class Vec[A]() extends FastMonoid[A, Vector[A]] {
    override implicit val monoid: Monoid[Vector[A]] = cats.instances.vector.catsKernelStdMonoidForVector[A]
    override def in(str: A): Vector[A] = Vector.empty :+ str
  }
}
