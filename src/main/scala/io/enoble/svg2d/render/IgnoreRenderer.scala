package io.enoble.svg2d.render

import cats.Monoid
import io.enoble.svg2d.ast.FastMonoid

final case class IgnoreRenderer[A, B](underlying: FastMonoid[A, Vector[B]]) extends FastMonoid[A, Vector[B]] {
  override implicit val monoid: Monoid[Vector[B]] =
    cats.instances.vector.catsKernelStdMonoidForVector[B]

  override def in(str: A): Vector[B] = Vector.empty
}
