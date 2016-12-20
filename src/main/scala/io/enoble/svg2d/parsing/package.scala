package io.enoble.svg2d


import scalaz._
import Scalaz._

package object parsing {

  type Model = PartialFunction[xml.Elem, Option[Vector[Code]]]
  type ParseError = Exception
  implicit class HasAttributes(val x: xml.Elem) extends AnyVal {
    def getOrDefault(n: String, default: String): String = x.attribute(n).map(_.head.text).getOrElse(default)
    def getOpt(n: String): Option[String] = x.attribute(n).map(_.head.text)
  }
  implicit class CaseInsensitiveEquals(val s: String) extends AnyVal {
    def ~=(other: String) = other.equalsIgnoreCase(s)
  }
  implicit class ParseDouble(val s: String) extends AnyVal {
    def asDouble = s.parseDouble.toOption
  }
  implicit class Ignorable[T](val t: T) extends AnyVal {
    def ignore = ()
  }
  implicit class CombinePartial[T, R](val partial: PartialFunction[T, R]) extends AnyVal {
    def makeTotal(total: (T) => R): T => R = partial orElse { case x => total(x) }
  }
  val parsers: List[Model] = List(CircleParser, EllipseParser, Text, PathParser, RectParser)

  implicit class JavaHelper(val sc: StringContext) extends AnyVal {
    def java(args: Any*): String = ("\n" + sc.parts.mkString).split("\n").mkString(";\n").trim()
  }
}
