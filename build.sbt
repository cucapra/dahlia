name := "Fuse"
version := "0.0.1"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1",
  "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.2.0",
  "com.github.scopt" %% "scopt" % "3.7.1",
  "com.outr" %% "scribe" % "2.7.2"
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

/* Define task to download picojson headers */
val getPicoJson = taskKey[Unit]("Download picojson header.")
getPicoJson := {
  import sys.process._
  import java.io.File
  import java.net.URL

  val picoJsonHdrLoc = new File("src/main/resources/headers/picojson.h")

  if (!picoJsonHdrLoc.exists()) {
    val picoJsonHdr = new URL("https://raw.githubusercontent.com/kazuho/picojson/master/picojson.h")
    val cmd = Seq("wget", picoJsonHdr, "--directory-prefix", picoJsonHdrLoc.toString)
    // sys.process DSL magic!
    picoJsonHdr #> picoJsonHdrLoc !!
  }
}

/* Override default assembly task to depend on getPicoJson */
assembly := {
  getPicoJson.value
  assembly.value
}

/* Running full test suite requires latest ./fuse build */
test := {
  assembly.value
  (test in Test).value
}
