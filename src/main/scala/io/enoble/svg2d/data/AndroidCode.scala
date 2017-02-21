package io
package enoble
package svg2d
package data

object AndroidCode {
  val empty = AndroidCode(Vector.empty)
  def apply(fragments: String*): AndroidCode =
    AndroidCode(Vector({ (sb: StringBuilder) => fragments.foreach { s => sb ++= s } }))
  def apply(fun: StringBuilder => Unit): AndroidCode =
    AndroidCode(Vector(fun))
}

case class AndroidCode(ap: RWRSBAction) extends AnyVal with Renderable {
  override def asString: String = {
    val builder = new StringBuilder()
    val () = ap.foreach(_ (builder))
    builder.result()
  }
}
