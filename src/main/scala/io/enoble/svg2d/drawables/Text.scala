package io.enoble.svg2d.drawables

import scala.xml.Elem

object Text extends Model {
  override def isDefinedAt(x: Elem): Boolean = x.label =~= "text"
  override def apply(v1: Elem): Option[(AndroidCode, IOSCode)] = {
    None
  }
}
