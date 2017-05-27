package io.enoble.svg2d.render

import io.enoble.svg2d.Steque
import io.enoble.svg2d.ast.FastMonoid

final case class IgnoreRenderer[A, B](underlying: FastMonoid[A, Steque[B]]) extends FastMonoid[A, Steque[B]] {
  override def empty: Steque[B] =
    Steque.empty
  override def append(x: Steque[B], y: Steque[B]): Steque[B] =
    x ++: y

  override def in(str: A): Steque[B] = Steque.empty
}
