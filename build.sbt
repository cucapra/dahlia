name := "Fuse"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1",
  "com.github.scopt" %% "scopt" % "3.5.0"
)

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature",
  "-Ywarn-unused",
  "-Ywarn-value-discard",
  "-Yno-adapted-args",
  "-Xfatal-warnings"
)
