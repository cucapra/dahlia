name := "Fuse"
version := "0.0.2"

scalaVersion := "2.13.0"

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

testOptions in Test += Tests.Argument("-oD")
parallelExecution in Test := false
logBuffered in Test := false

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
