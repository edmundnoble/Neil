package io.enoble.svg2d.parsing

import com.squareup.javapoet.MethodSpec

import scala.language.higherKinds
import scala.xml.Elem
import scalaz.Free.Trampoline
import scalaz._
import Scalaz._

object Parse {
  def traverse[A, T[_]](f: (xml.Elem) => T[A])(x: xml.Elem)(implicit M: Monoid[T[A]]): T[A] = {
    def inner(x: xml.Node): Trampoline[T[A]] = {
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
    traverse((e: xml.Elem) => Vector(e.label))(x).foldLeft(Map[String, Int]()) {
      (map, str) => map + (str -> (map.getOrElse(str, 0) + 1))
    }
  }

  def pathStats(x: xml.Elem): Map[Char, Int] = {
    traverse((e: xml.Elem) => if (e.label == "path") Vector(e.attribute("d").get.head.text.filter(c => c.isLetter)) else Vector.empty[String])(x).flatten.foldLeft(Map[Char, Int]()) {
      (map, str) => map + (str -> (map.getOrElse(str, 0) + 1))
    }
  }

  def attributeStats(x: xml.Elem): Map[String, Int] = {
    val attrs = traverse((e: xml.Elem) => e.attributes.asAttrMap.keys.toVector)(x)
    attrs.foldLeft(Map[String, Int]()) {
      (map, str) => map + (str -> (map.getOrElse(str, 0) + 1))
    }
  }

  def parseAll: xml.Elem => Option[Code] = traverse(parseDrawable)

  def parseDrawable: xml.Elem => Option[Code] = parsers.reduce(_ orElse _) makeTotal (_ => None)
}
