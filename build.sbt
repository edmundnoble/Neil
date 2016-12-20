scalaVersion := "2.11.8"

libraryDependencies += "org.scalaz" % "scalaz-core_2.11" % "7.2.8"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "3.0.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6"

libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.2.1"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.3.0"

resolvers += Resolver.sonatypeRepo("public")

javacOptions += "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=5005"

fork in test := true