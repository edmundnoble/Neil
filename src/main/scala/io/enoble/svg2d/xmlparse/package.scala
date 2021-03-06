package io
package enoble
package svg2d

import scala.util.Try
import scala.xml.MetaData

package object xmlparse {

  type ParseError = Exception

  implicit class HasAttributes(val x: MetaData) extends AnyVal {
    def getOrDefault(n: String, default: String): String = x.get(n).map(_.head.text).getOrElse(default)

    def getOpt(n: String): Option[String] = x.get(n).map(_.head.text)
  }

  implicit class CaseInsensitiveEquals(val s: String) extends AnyVal {
    def ~=(other: String): Boolean = other.equalsIgnoreCase(s)
  }

  implicit class ParseDouble(val s: String) extends AnyVal {
    def asDouble: Option[Double] = Try(s.toDouble).toOption
  }

  implicit class Ignorable[T](val t: T) extends AnyVal {
    def ignore(): Unit = ()
  }

  implicit class CombinePartial[T, R](val partial: PartialFunction[T, R]) extends AnyVal {
    def makeTotal(total: (T) => R): T => R = partial orElse { case x => total(x) }
  }

  implicit class JavaHelper(val sc: StringContext) extends AnyVal {
    def java(args: Any*): String = ("\n" + sc.parts.mkString).split("\n").mkString(";\n").trim()
  }

}
