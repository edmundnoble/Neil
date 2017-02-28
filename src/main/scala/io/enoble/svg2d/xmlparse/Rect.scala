package io
package enoble
package svg2d
package xmlparse

import io.enoble.svg2d.ast.FinalSVG

import scala.xml.MetaData

object Rect extends Model {
  override val label: String = "rect"
  override def apply[A](v1: MetaData, svg: FinalSVG[A]): Option[A] = {
//    System.err.println("Tried to parse a rect!")
    None
  }
}

