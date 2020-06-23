name := "Fuse"
version := "0.0.2"

scalaVersion := "2.13.2"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "com.github.scopt" %% "scopt" % "3.7.1",
  "com.outr" %% "scribe" % "2.7.9",
  "com.lihaoyi" %% "sourcecode" % "0.1.7"
)

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature",
  "-Ywarn-unused",
  "-Ywarn-value-discard",
  "-Xfatal-warnings"
)

// Disable options in sbt console.
scalacOptions in (Compile, console) ~=
  (_ filterNot ((Set("-Xfatal-warnings", "-Ywarn-unused").contains(_))))

testOptions in Test += Tests.Argument("-oD")
parallelExecution in Test := false
logBuffered in Test := false

/* Store commit hash information */
resourceGenerators in Compile += Def.task {
  import scala.sys.process._
  val file = (resourceManaged in Compile).value / "version.properties"
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
assemblyOption in assembly := (assemblyOption in assembly).value.copy(
    prependShellScript = Some(sbtassembly.AssemblyPlugin.defaultShellScript)
)
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
    val jsonHpp = new URL(
      "https://raw.githubusercontent.com/nlohmann/json/develop/single_include/nlohmann/json.hpp")
    val cmd = Seq("wget", jsonHpp, "--directory-prefix", jsonHppLoc.toString)
    // sys.process DSL magic!
    jsonHpp #> jsonHppLoc !!
  }
}

/* Override default assembly task to depend on getHeaders */
assembly := {
  getHeaders.value
  assembly.value
}

Test / compile := {
  getHeaders.value
  (Test / compile).value
}
