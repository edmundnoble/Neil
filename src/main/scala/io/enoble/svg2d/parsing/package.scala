package io.enoble.svg2d

import com.squareup.javapoet.MethodSpec
import scalaz._
import Scalaz._

package object parsing {
  type AndroidCode = String
  type IOSCode = String
  type Model = PartialFunction[xml.Elem, Option[Code]]
  type ParseError = Exception
  implicit class HasAttributes(val x: xml.Elem) extends AnyVal {
    def getOrDefault(n: String, default: String): String = x.attribute(n).map(_.head.text).getOrElse(default)
  }
  implicit class CaseInsensitiveEquals(val s: String) extends AnyVal {
    def =~=(other: String) = other.equalsIgnoreCase(s)
  }
  implicit class ParseDouble(val s: String) extends AnyVal {
    def asDouble = s.parseDouble.toOption
  }
  implicit class Ignorable[T](val t: T) extends AnyVal {
    def ignore = ()
  }
  implicit class CombinePartial[T, R](val partial: PartialFunction[T, R]) extends AnyVal {
    def makeTotal(total: (T) => R): T => R = { p =>
      if (partial.isDefinedAt(p))
        partial(p)
      else
        total(p)
    }
  }
  val parsers = List(CircleParser, EllipseParser, Text)
  implicit val codeInstances = CodeInstances
}
