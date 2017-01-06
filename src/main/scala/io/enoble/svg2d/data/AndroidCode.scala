package io.enoble.svg2d.data

object AndroidCode {
  def apply(fragments: String*): AndroidCode =
    AndroidCode(fragments.toVector)
}

case class AndroidCode(fragments: Vector[String]) extends Renderable {
  override def asString: String = fragments.filter(_.nonEmpty).mkString(";\n")
}
