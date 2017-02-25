package io
package enoble
package svg2d
package render

import java.io.PrintStream

import cats.Monoid
import io.enoble.svg2d.ast.FastMonoid
import monix.eval.Coeval

final case class PrintRenderer[A, B](out: PrintStream, underlying: FastMonoid[A, B]) extends FastMonoid[A, Vector[() => Unit]] {
  override implicit val monoid: Monoid[Vector[() => Unit]] =
    cats.instances.vector.catsKernelStdMonoidForVector[() => Unit]

  override def in(str: A): Vector[() => Unit] =
    Vector.empty :+ (() => out.print(underlying.in(str)))
}
