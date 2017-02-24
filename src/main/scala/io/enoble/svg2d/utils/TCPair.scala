package io
package enoble
package svg2d
package utils

final case class TCPair[T[_], A](t: T[A], a: A)
sealed abstract class TCPairC[F[_], T[_]] {
  type X
  val a: F[X]
  val t: T[X]
}

object TCPairC {
  def apply[F[_], T[_], A](fa: F[A])(implicit ta: T[A]): TCPairC[F, T] = new TCPairC[F, T] {
    override type X = A
    override val a: F[A] = fa
    override val t: T[A] = ta
  }
}
