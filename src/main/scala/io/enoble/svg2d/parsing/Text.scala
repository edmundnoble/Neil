package io.enoble.svg2d.parsing

import io.enoble.svg2d.parsing.Code.Named

import scala.xml.Elem

import scalaz._
import Scalaz._

object Text extends Model {
  override def isDefinedAt(x: Elem): Boolean = x.label =~= "text"
  override def apply(v1: Elem): Option[Vector[Code]] = {
    for {
      posX <- v1.getOpt("x").flatMap(_.asDouble)
      posY <- v1.getOpt("y").flatMap(_.asDouble)
      text = v1.getOrDefault("text", "")
    } yield Vector(DrawText(text, posX, posY))
  }
}

case class DrawText(text: String, posX: Double, posY: Double) extends Code {
  override def toAndroidCode: Named[AndroidCode] = AndroidCode(s"c.drawText($text, $posX, $posY, p)").pure[Named]

  override def toIOSCode: Named[IOSCode] = ???
}