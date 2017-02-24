package io
package enoble
package svg2d
package xmlparse

import io.enoble.svg2d.ast.FinalSVG
import io.enoble.svg2d.utils.Named.Named

import scala.xml.Elem


object Text extends Model {
  override val label: String = "text"
  override def apply[A](v1: Elem, ctx: FinalSVG[A]): Option[A] = {
      for {
        posX <- v1.getOpt("x").flatMap(_.asDouble)
        posY <- v1.getOpt("y").flatMap(_.asDouble)
        text = v1.getOrDefault("text", "")
      } yield ctx.text(text, posX, posY)
  }
}
