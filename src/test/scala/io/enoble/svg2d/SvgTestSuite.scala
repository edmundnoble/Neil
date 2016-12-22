package io.enoble.svg2d

import io.enoble.svg2d.parsing.Parse
import io.enoble.svg2d.render.{InitialCode, InitialRenderer}
import org.scalatest.FunSuite

abstract class SvgTestSuite extends FunSuite {
  def verifySingle(svg: String, cmd: InitialCode): Unit = {
    val svgXml = xml.XML.loadString(svg)
    assert(Parse.parseAll(InitialRenderer)(svgXml) === Some(cmd))
  }
}