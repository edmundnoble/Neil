name in ThisBuild := "neil"

scalaVersion in ThisBuild := "2.12.2"

version in ThisBuild := "0.0.2"

scalacOptions ++= Seq(
  "-Xlint",
  "-opt:l:classpath",
  "-Ypartial-unification",
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-Xfatal-warnings"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

libraryDependencies += "org.typelevel" %% "cats" % "0.9.0"

libraryDependencies += "io.monix" %% "monix" % "2.2.2"

libraryDependencies += "io.monix" %% "monix-cats" % "2.2.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6"

libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.4.3"

libraryDependencies += "com.github.alexarchambault" %% "case-app" % "1.2.0-M1"

test in assembly := {}

mainClass in assembly := Some("io.enoble.svg2d.Main")

resolvers += Resolver.sonatypeRepo("public")

javaOptions in run ++=
  Seq("-Xmx1g", "-Xms1g")

val timeTask: TaskKey[Unit] = TaskKey("time")
timeTask := { (run in Compile).toTask(" -a android -i svg --timed overall:300").value }

fork in test := true
fork in (run in Compile) := true

