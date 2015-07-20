import io.enoble.svg2d.parsing.{Parse, Code}
import org.scalatest.FunSuite

class SvgTestSuite extends FunSuite {
  def verifySingle(svg: String, cmd: Code): Unit = {
    val svgXml = xml.XML.loadString(svg)
    assert(Parse.parseAll(svgXml) === Some(cmd))
  }
}