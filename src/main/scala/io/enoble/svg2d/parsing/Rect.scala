package io.enoble.svg2d.parsing

import io.enoble.svg2d.parsing.Code.Named

object RectParser extends Model {
  override def isDefinedAt(x: xml.Elem): Boolean = x.label =~= "rect"
  override def apply(v1: xml.Elem): Option[Code] = {
    System.err.println("Tried to parse a rect!")
    None
  }
}

case class Rect() extends Code {
  override def toAndroidCode: Named[AndroidCode] = ???
  override def toIOSCode: Named[IOSCode] = ???
}
