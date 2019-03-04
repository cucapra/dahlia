name := "Fuse"
version := "0.0.1"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1",
  "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.2.0",
  "com.github.scopt" %% "scopt" % "3.7.1"
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

testOptions in Test += Tests.Argument("-oD")
parallelExecution in Test := false

assemblyJarName in assembly := "fuse.jar"
test in assembly := {}
