package io.enoble.svg2d.data

import scalaz.Monoid

case class SwiftCode(fragments: Vector[String]) extends Renderable {
  override def asString: String = fragments.filter(_.nonEmpty).mkString("\n")
}

case class ObjectiveCCode(fragments: Vector[String]) extends Renderable {
  override def asString: String = fragments.filter(_.nonEmpty).mkString(";\n")
}

