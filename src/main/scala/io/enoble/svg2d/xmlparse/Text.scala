package io.enoble.svg2d.xmlparse

import io.enoble.svg2d.ast.FinalSVG
import io.enoble.svg2d.utils.Named.Named

import scala.xml.Elem
import scalaz._
import Scalaz._

object Text extends Model {
  override def apply[A](v1: Elem, ctx: FinalSVG[A]): Option[Option[A]] = {
    if (v1.label ~= "text") {
      for {
        posX <- v1.getOpt("x").flatMap(_.asDouble)
        posY <- v1.getOpt("y").flatMap(_.asDouble)
        text = v1.getOrDefault("text", "")
      } yield Some(ctx.text(text, posX, posY))
    } else {
      None
    }
  }
}
