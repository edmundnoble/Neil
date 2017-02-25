package io.enoble.svg2d.render

import cats.Monoid
import io.enoble.svg2d.ast.FastMonoid

final case class IgnoreRenderer[A](underlying: FastMonoid[A, Vector[() => Unit]]) extends FastMonoid[A, Vector[() => Unit]] {
  override implicit val monoid: Monoid[Vector[() => Unit]] =
    cats.instances.vector.catsKernelStdMonoidForVector[() => Unit]

  override def in(str: A): Vector[() => Unit] = Vector.empty
}
