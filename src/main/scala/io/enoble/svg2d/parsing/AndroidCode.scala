package io.enoble.svg2d.parsing

import scalaz._
import Scalaz._

object AndroidCode {

  def appendAndroid(str: String, other: String): String =
    if (other.trim.length == 0 || str.trim.length == 0) str + "\n" + other
    else str + ";\n" + other

  trait AndroidInstances extends Monoid[AndroidCode] {
    override def zero: AndroidCode = AndroidCode("")

    override def append(f1: AndroidCode, f2: => AndroidCode): AndroidCode = AndroidCode(appendAndroid(f1.unsafe, f2.unsafe))
  }

  def apply(strs: String*): AndroidCode = AndroidCode(strs.reduce(appendAndroid))
}

case class AndroidCode(private val unsafe: String) extends AnyVal {
  import AndroidCode._

  def asString: String = unsafe + ";\n"
}
