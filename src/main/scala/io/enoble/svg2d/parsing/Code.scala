package io.enoble.svg2d.parsing

import scalaz._
import Scalaz._

abstract class Code {

  import Code._

  def toAndroidCode: Named[AndroidCode]

  def toIOSCode: Named[IOSCode]

  def freshName(prefix: String): Named[String] = for {
    nameCounts <- get[Map[String, Int]]
    count = nameCounts.getOrElse(prefix, 0) + 1
    newName = prefix + count
    _ <- put(nameCounts + (prefix -> count))
  } yield newName

  def currentName(prefix: String): Named[String] = for {
    nameCounts <- get[Map[String, Int]]
    count = nameCounts.getOrElse(prefix, 0)
    current = prefix + count
  } yield current
}

object Code {

  type Named[A] = State[Map[String, Int], A]
  def empty = new Code {
    def toAndroidCode = AndroidCode("").pure[Named]
    def toIOSCode = "".pure[Named]
  }
}

object CodeInstances extends Monoid[Code] {
  override def append(c: Code, l: => Code) = new Code {
    def toAndroidCode = for {
      codeC <- c.toAndroidCode
      codeL <- l.toAndroidCode
    } yield codeC |+| codeL
    def toIOSCode = for {
      codeC <- c.toIOSCode
      codeL <- l.toIOSCode
    } yield s"$codeC\n$codeL"
  }
  override def zero = Code.empty
}


