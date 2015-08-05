package io.enoble.svg2d.parsing

import scala.language.higherKinds
import scala.xml.Elem
import scalaz.Free.Trampoline
import scalaz._
import Scalaz._

object Parse {
  def foldXml[A, B](f: (xml.Elem) => B)(x: xml.Elem)(implicit M: Monoid[B]): B = {
    def inner(x: xml.Node): Trampoline[B] = {
      val res = x match {
        case x1: Elem =>
          f(x1)
        case _ =>
          M.zero
      }
      val tramp = x.child.foldLeft(Trampoline.delay(M.zero)) { (acc, x) =>
        for {
          rep <- Trampoline.suspend(inner(x))
          prev <- acc
        } yield prev |+| rep
      }
      tramp.map(_ |+| res)
    }
    inner(x).run
  }

  def elementStats(x: xml.Elem): Map[String, Int] = {
    foldXml((e: xml.Elem) => Vector(e.label))(x).elemCount
  }

  implicit class ElemCountOps[T](val x: Seq[T]) {
    def elemCount = x.foldLeft(Map[T, Int]()) { case (map, key) => map + (key -> (map.getOrElse(key, 0) + 1)) }
  }

  def pathStats(x: xml.Elem): Map[Char, Int] = {
    val pathCommands = foldXml((e: xml.Elem) =>
      if (e.label == "path") e.attribute("d").get.head.text.filter(c => c.isLetter).toVector
      else Vector.empty[Char]
    )(x)
      pathCommands.elemCount
  }

  def attributeStats(x: xml.Elem): Map[String, Int] = {
    val attrs = foldXml((e: xml.Elem) => e.attributes.asAttrMap.keys.toVector)(x)
    attrs.elemCount
  }

  def parseAll: xml.Elem => Option[Code] = foldXml(parseDrawable)

  def parseDrawable: xml.Elem => Option[Code] = parsers.reduce(_ orElse _) makeTotal (_ => None)
}
