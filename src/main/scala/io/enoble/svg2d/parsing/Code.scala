package io.enoble.svg2d.parsing

import scalaz.Monoid

trait Code {
  def toAndroidCode: AndroidCode
  def toIOSCode: IOSCode
}

object Code {
  def empty = new Code {
    def toAndroidCode = ""
    def toIOSCode = ""
  }
}

object CodeInstances extends Monoid[Code] {
  override def append(c: Code, l: => Code) = new Code {
    def toAndroidCode = s"${c.toAndroidCode}\n${l.toAndroidCode}"
    def toIOSCode = s"${c.toIOSCode}\n${l.toIOSCode}"
  }
  override def zero = Code.empty
}


