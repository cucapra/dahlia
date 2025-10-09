name := "Dahlia"
version := "0.0.2"

scalaVersion := "3.3.1"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.18" % "test",
  "org.scalatest" %% "scalatest-funspec" % "3.2.18" % "test",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.0.0",
  "com.lihaoyi" %% "fastparse" % "3.0.2",
  "com.github.scopt" %% "scopt" % "4.0.1",
  "com.outr" %% "scribe" % "3.5.5",
  "com.lihaoyi" %% "sourcecode" % "0.2.7",
  "com.lihaoyi" %% "upickle" % "4.1.0"
)

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature",
  "-Xfatal-warnings",
  "-new-syntax",
  "-indent"
)

// Reload changes to this file.
Global / onChangedBuildSource := ReloadOnSourceChanges

// Disable options in sbt console.
Compile / console / scalacOptions ~=
  (_ filterNot ((Set("-Xfatal-warnings", "-Ywarn-unused").contains(_))))

Test / testOptions += Tests.Argument("-oD")
Test / parallelExecution := false
Test / logBuffered := false

/* Store commit hash information */
Compile / resourceGenerators += Def.task {
  import scala.sys.process._
  val file = (Compile / resourceManaged).value / "version.properties"
  val gitHash = "git rev-parse --short HEAD".!!
  val gitDiff = "git diff --stat".!!
  val status = if (gitDiff.trim() != "") "dirty" else "clean"
  println(gitDiff)
  IO.writeLines(file, Seq(
    s"git.status = $status",
    s"build.date = ${new java.util.Date()}",
    s"git.hash = $gitHash"))
  Seq(file)
}

/* sbt-assembly configuration: build an executable jar. */
ThisBuild / assemblyPrependShellScript := Some(sbtassembly.AssemblyPlugin.defaultShellScript)
assembly / assemblyJarName := "fuse.jar"
assembly / test := {}

/* Define task to download picojson headers */
val getHeaders = taskKey[Unit]("Download header dependencies for runnable backend.")
getHeaders := {
  import sys.process._
  import java.io.File
  import java.net.URL

  val jsonHppLoc = new File("src/main/resources/headers/json.hpp")

  if (!jsonHppLoc.exists()) {
    val jsonHpp = new URL(
      "https://raw.githubusercontent.com/nlohmann/json/develop/single_include/nlohmann/json.hpp")
    val cmd = Seq("wget", jsonHpp, "--directory-prefix", jsonHppLoc.toString)
    // sys.process DSL magic!
    jsonHpp #> jsonHppLoc !!
  }
}

Test / compile := {
  (Test / compile).value
}
