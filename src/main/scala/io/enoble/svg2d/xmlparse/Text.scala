package io
package enoble
package svg2d
package xmlparse

import io.enoble.svg2d.ast.FinalSVG
import io.enoble.svg2d.utils.Named.Named

import scala.xml.{Elem, MetaData}


object Text extends TerminalModel {
  override val label: String = "text"
  override def apply[A](v1: MetaData, text: String, ctx: FinalSVG[A]): Option[A] = {
      for {
        posX <- v1.getOpt("x").flatMap(_.asDouble)
        posY <- v1.getOpt("y").flatMap(_.asDouble)
      } yield ctx.text(text, posX, posY)
  }
}
