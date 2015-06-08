package io.enoble.svg2d.parsing

import com.squareup.javapoet.MethodSpec.Builder

import scala.xml.Elem
import scalaz._
import Scalaz._

object CircleParser extends Model {
  override def isDefinedAt(x: Elem): Boolean = x.label =~= "circle"
  override def apply(v1: Elem): Option[Code] = {
    val centerX = v1.getOrDefault("cx", "0")
    val centerY = v1.getOrDefault("cy", "0")
    val radius = v1.getOrDefault("r", "0")

    if (radius == "0") {
      Some(Code.empty)
    } else {
      for {x <- centerX.asDouble if x >= 0
           y <- centerY.asDouble if y >= 0
           r <- radius.asDouble
      } yield DrawCircle(x, y, r)
    }
  }
}

case class DrawCircle(x: Double, y: Double, r: Double) extends Code {
  override def toAndroidCode: AndroidCode = s"c.drawCircle($x, $y, $r, p);"
  override def toIOSCode: IOSCode = s""
}
