package io.enoble.svg2d.data

import scalaz._

object AndroidCode {
  def empty = AndroidCode(Vector.empty)

  implicit val androidMonoid = new Monoid[AndroidCode] {
    override def zero: AndroidCode = empty
    override def append(f1: AndroidCode, f2: => AndroidCode): AndroidCode =
      AndroidCode(f1.fragments ++ f2.fragments)
  }

  def apply(fragments: String*): AndroidCode =
    AndroidCode(fragments.toVector)
}

case class AndroidCode(fragments: Vector[String]) extends Renderable {
  override def asString: String = fragments.filter(_.nonEmpty).mkString(";\n")
}
