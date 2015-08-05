package io.enoble.svg2d


import scalaz._
import Scalaz._

package object parsing {

  implicit class CodeBuildingOps(val str: String) extends AnyVal {
    def +|+(other: String) = if (other.trim.length == 0 || str.trim.length == 0) str + "\n" + other else str + ";\n" + other
  }
  implicit object AndroidInstances extends Monoid[AndroidCode] {
    override def zero: AndroidCode = AndroidCode("")
    override def append(f1: AndroidCode, f2: => AndroidCode): AndroidCode = AndroidCode(f1.unsafe +|+ f2.unsafe)
  }

  case class AndroidCode(unsafe: String) extends AnyVal {
    def asString: String = unsafe +|+ ";\n"
  }

  object AndroidCode {
    def apply(strs: String*): AndroidCode = AndroidCode(strs.reduce(_ +|+ _))
  }

  type IOSCode = String
  type Model = PartialFunction[xml.Elem, Option[Code]]
  type ParseError = Exception
  implicit class HasAttributes(val x: xml.Elem) extends AnyVal {
    def getOrDefault(n: String, default: String): String = x.attribute(n).map(_.head.text).getOrElse(default)
    def getOpt(n: String): Option[String] = x.attribute(n).map(_.head.text)
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
    def makeTotal(total: (T) => R): T => R = partial orElse { case x => total(x) }
  }
  val parsers: List[Model] = List(CircleParser, EllipseParser, Text, PathParser, RectParser)
  implicit val codeInstances = CodeInstances
  implicit class JavaHelper(val sc: StringContext) extends AnyVal {
    def java(args: Any*): String = ("\n" + sc.parts.mkString).split("\n").mkString(";\n").trim()
  }
}
