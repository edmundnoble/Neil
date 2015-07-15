package io.enoble.svg2d.parsing

import scala.xml.Elem

object Text extends Model {
  override def isDefinedAt(x: Elem): Boolean = x.label =~= "text"
  override def apply(v1: Elem): Option[Code] = {
    for {
      posX <- v1.getOpt("x").flatMap(_.asDouble)
      posY <- v1.getOpt("y").flatMap(_.asDouble)
      text = v1.getOrDefault("text", "")
    } yield DrawText(text, posX, posY)
  }
}

case class DrawText(text: String, posX: Double, posY: Double) extends Code {
  override def toAndroidCode: AndroidCode = s"c.drawText($text, $posX, $posY, p);"

  override def toIOSCode: IOSCode = ???
}