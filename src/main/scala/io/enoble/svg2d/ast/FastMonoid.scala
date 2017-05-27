package io
package enoble
package svg2d
package ast

import cats.Monoid

trait FastMonoid[S, A] {
  def append(a1: A, a2: A): A

  def empty: A

  // intended to be a monoid homomorphism, up to observation.
  def in(str: S): A
}

object FastMonoid {

  final case class Id[A]()(implicit val monoid: Monoid[A]) extends FastMonoid[A, A] {
    override def in(a: A): A = a
    override def append(a1: A, a2: A): A = monoid.combine(a1, a2)
    override def empty: A = monoid.empty
  }

  final case class ToString[A]() extends FastMonoid[A, Vector[String]] {
    override def in(a: A): Vector[String] = Vector.empty :+ a.toString
    override def append(a1: Vector[String], a2: Vector[String]): Vector[String] = a1 ++ a2
    override def empty: Vector[String] = Vector.empty
  }

  final case class Tq[A]() extends FastMonoid[A, Steque[A]] {
    override def empty: Steque[A] = Steque.empty
    override def append(x: Steque[A], y: Steque[A]): Steque[A] = x ++: y
    override def in(str: A): Steque[A] = Steque.empty :+ str
  }
}
