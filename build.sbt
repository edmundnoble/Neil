name in ThisBuild := "neil"

scalaVersion in ThisBuild := "2.11.8"

scalaOrganization in ThisBuild := "org.typelevel"

version in ThisBuild := "0.0.2"

scalacOptions ++= Seq(
  "-Xlint",
  "-Ypartial-unification",
  "-Yliteral-types",
  "-feature",
  "-language:higherKinds",
  "-Xfatal-warnings"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

libraryDependencies += "org.typelevel" %% "cats" % "0.9.0"

libraryDependencies += "io.monix" %% "monix" % "2.2.1"

libraryDependencies += "io.monix" %% "monix-cats" % "2.2.1"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "3.0.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6"

libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.4.1"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.3.0"

resolvers += Resolver.sonatypeRepo("public")

javacOptions += "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=5005"

fork in test := true