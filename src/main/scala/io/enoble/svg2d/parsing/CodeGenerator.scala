package io.enoble.svg2d.parsing

import scalaz._
import Scalaz._

import Code._

object CodeGenerator {
  def generateAndroidCode(parsed: Vector[Code]): String = {
    parsed.foldLeft(AndroidCode.empty) { (file, code) =>
      file |+| code.toAndroidCode.eval(Map())
    }.asString
  }

  def generateObjCCode(parsed: Vector[Code]): String = {
    Tag.unwrap(parsed.foldLeft(IOSCode.empty) { (file, code) =>
      file |+| code.toIOSCode.eval(Map())
    }.objectiveCGen.run)
  }

  def generateSwiftCode(parsed: Vector[Code]): String = {
    Tag.unwrap(parsed.foldLeft(IOSCode.empty) { (file, code) =>
      file |+| code.toIOSCode.eval(Map())
    }.swiftGen.run)
  }

}
