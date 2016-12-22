package io.enoble.svg2d.data

import scalaz.Monoid

case class SwiftCode(fragments: Vector[String]) extends Renderable {
  override def asString: String = fragments.filter(_.nonEmpty).mkString("\n")
}
case class ObjectiveCCode(fragments: Vector[String]) extends Renderable {
  override def asString: String = fragments.filter(_.nonEmpty).mkString(";\n")
}

object SwiftCode {
  val empty = SwiftCode(Vector.empty)
  implicit val swiftMonoid = new Monoid[SwiftCode] {
    override def zero: SwiftCode = empty
    override def append(f1: SwiftCode, f2: => SwiftCode): SwiftCode =
      SwiftCode(f1.fragments ++ f2.fragments)
  }
}

object ObjectiveCCode {
  val empty = ObjectiveCCode(Vector.empty)
  implicit val objectiveCMonoid = new Monoid[ObjectiveCCode] {
    override def zero: ObjectiveCCode = empty
    override def append(f1: ObjectiveCCode, f2: => ObjectiveCCode): ObjectiveCCode =
      ObjectiveCCode(f1.fragments ++ f2.fragments)
  }
}

