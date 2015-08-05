package io.enoble.svg2d

import io.enoble.svg2d.parsing.{Code, Parse}
import org.scalatest.FunSuite

abstract class SvgTestSuite extends FunSuite {
  def verifySingle(svg: String, cmd: Code): Unit = {
    val svgXml = xml.XML.loadString(svg)
    assert(Parse.parseAll(svgXml) === Some(cmd))
  }
}