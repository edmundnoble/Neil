package io.enoble.svg2d

final case class Surrounders(prefixFromName: String => String, suffix: String)

object Surrounders {

  def androidPrefix(fileName: String): String = {
    val escapedFileName = fileName
    s"""class SVGView_$escapedFileName extends View {
    @Override
    public void onDraw(Graphics g) {
"""
  }

  def androidSuffix: String =
    """    }
}"""

  val android = Surrounders(androidPrefix, androidSuffix)
  val swift = Surrounders(_ => "", "")
  val objc = Surrounders(_ => "", "")

}
