package io
package enoble
package svg2d
package render

import java.io.PrintStream

import cats.Monoid
import io.enoble.svg2d.ast.FastMonoid
import monix.eval.Coeval

final case class PrintRenderer[A, B](out: PrintStream, underlying: FastMonoid[A, B]) extends FastMonoid[A, Vector[Coeval[Unit]]] {
  override implicit val monoid: Monoid[Vector[Coeval[Unit]]] =
    new Monoid[Vector[Coeval[Unit]]] {
      override def empty: Vector[Coeval[Unit]] =
        Vector.empty
      override def combine(x: Vector[Coeval[Unit]], y: Vector[Coeval[Unit]]): Vector[Coeval[Unit]] =
        x ++ y
    }

  override def in(str: A): Vector[Coeval[Unit]] =
    Vector.empty :+ Coeval.eval(out.print(underlying.in(str)))
}
