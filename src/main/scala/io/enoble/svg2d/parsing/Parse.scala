package io.enoble.svg2d.parsing

import io.enoble.svg2d.ast.FinalSVG

import scala.language.higherKinds
import scala.xml.Elem
import scalaz.Free.Trampoline
import scalaz._
import Scalaz._

object Parse {
  def foldXml[B](f: (xml.Elem) => B)(x: xml.Elem)(implicit M: Monoid[B]): B = {
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

  def parseTree[A](svg: FinalSVG[A], f: xml.Elem => Option[Option[A]])(x: xml.Elem): Option[Option[A]] = {
    f(x).orElse {
      x.child match {
        case xs if xs.isEmpty => Some(None)
        case xs =>
          xs.foldLeft[Option[Option[A]]](Some(None)) { (res1, res2) =>
            val second = res2 match {
              case x1: Elem => parseTree(svg, f)(x1)
              case _ => Some(None)
            }
            (res1 |@| second){(a, va) => implicit val sem = svg.monoid[A]; a |+| va}
          }
      }
    }
  }

  def elementStats(x: xml.Elem): Map[String, Int] = {
    foldXml((e: xml.Elem) => Vector(e.label))(x).elemCount
  }

  implicit class ElemCountOps[T](val x: Seq[T]) {
    def elemCount: Map[T, Int] =
      x.foldLeft(Map[T, Int]()) { case (map, key) => map + (key -> (map.getOrElse(key, 0) + 1)) }
  }

  def pathStats(elem: xml.Elem): Map[Char, Int] = {
    val pathCommands = foldXml((e: xml.Elem) =>
      if (e.label == "path") e.attribute("d").get.head.text.filter(c => c.isLetter).toVector
      else Vector.empty[Char]
    )(elem)
    pathCommands.elemCount
  }

  def attributeStats(elem: xml.Elem): Map[String, Int] = {
    val attrs = foldXml((e: xml.Elem) => e.attributes.asAttrMap.keys.toVector)(elem)
    attrs.elemCount
  }

  def parseAll[A](svg: FinalSVG[A]): xml.Elem => Option[Option[A]] =
    parseTree[A](svg, parseDrawable[A](svg))

  def parseDrawable[A](svg: FinalSVG[A]): xml.Elem => Option[Option[A]] = elem => parsers.reduce((p1, p2) => new Model {
    def apply[R](elem: xml.Elem, svg: FinalSVG[R]): Option[Option[R]] = p1(elem, svg) orElse p2(elem, svg)
  }).apply(elem, svg).orElse {
    println(s"Unrecognized element: ${elem.label}")
    None
  }

  val parsers: List[Model] =
    List(Circle, Ellipse, Text, PathParser, RectParser)
}
