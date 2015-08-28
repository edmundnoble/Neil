package io.enoble.svg2d.parsing

import scalaz._
import Scalaz._

object AndroidCode {

  implicit class CodeBuildingOps(val str: String) extends AnyVal {
    def +|+(other: String) = if (other.trim.length == 0 || str.trim.length == 0) str + "\n" + other else str + ";\n" + other
  }

  trait AndroidInstances extends Monoid[AndroidCode] {
    override def zero: AndroidCode = AndroidCode("")

    override def append(f1: AndroidCode, f2: => AndroidCode): AndroidCode = AndroidCode(f1.unsafe +|+ f2.unsafe)
  }

  def apply(strs: String*): AndroidCode = AndroidCode(strs.reduce(_ +|+ _))
}

case class AndroidCode(unsafe: String) extends AnyVal {
  import AndroidCode._

  def asString: String = unsafe +|+ ";\n"
}
