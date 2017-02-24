package io
package enoble
package svg2d
package utils

import cats.data.State

object Named {
  type Named[A] = State[Map[String, Int], A]

  def freshName(prefix: String): Named[String] =
    State[Map[String, Int], String] { nameCounts =>
      val count = nameCounts.getOrElse(prefix, 0) + 1
      val newName = prefix + count
      (nameCounts + (prefix -> count), newName)
    }

  def currentName(prefix: String): Named[String] = for {
    nameCounts <- State.get[Map[String, Int]]
    count = nameCounts.getOrElse(prefix, 0)
    current = prefix + count
  } yield current

}


