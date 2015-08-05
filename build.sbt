scalaVersion := "2.11.6"

libraryDependencies += "org.scalaz" % "scalaz-core_2.11" % "7.2.0-M1"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "3.0.0-SNAP5"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.4"

libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.2.1"

javacOptions += "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=5005"

fork in test := true