name := "Fuse"
version := "0.0.1"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1",
  "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.2.0",
  "com.github.scopt" %% "scopt" % "3.7.1",
  "com.outr" %% "scribe" % "2.7.2",
  "com.lihaoyi" %% "sourcecode" % "0.1.5"
)

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature",
  // "-Ywarn-unused",
  // "-Ywarn-value-discard",
  "-Yno-adapted-args",
  // "-Xfatal-warnings"
)

testOptions in Test += Tests.Argument("-oD")
parallelExecution in Test := false
logBuffered in Test := false

assemblyJarName in assembly := "fuse.jar"
test in assembly := {}

/* Define task to download picojson headers */
val getHeaders = taskKey[Unit]("Download header dependencies for runnable backend.")
getHeaders := {
  import sys.process._
  import java.io.File
  import java.net.URL

  val jsonHppLoc = new File("src/main/resources/headers/json.hpp")

  if (!jsonHppLoc.exists()) {
    val jsonHpp = new URL("https://raw.githubusercontent.com/nlohmann/json/develop/single_include/nlohmann/json.hpp")
    val cmd = Seq("wget", jsonHpp, "--directory-prefix", jsonHppLoc.toString)
    // sys.process DSL magic!
    jsonHpp #> jsonHppLoc !!
  }
}

/* Override default assembly task to depend on getPicoJson */
assembly := {
  getHeaders.value
  assembly.value
}
