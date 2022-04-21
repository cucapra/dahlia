name := "Dahlia"
ThisBuild / version := "0.0.2"

lazy val shared = (project in file("shared/"))
  .settings(
    scalaVersion := "3.1.2",
    scalacOptions ++= Seq("-new-syntax", "-indent"),
  )
lazy val parser = (project in file("parser/"))
  .dependsOn(shared)
  .settings(
    scalaVersion := "2.13.7",
    scalacOptions += "-Ytasty-reader"
  )
lazy val root = (project in file("."))
  .dependsOn(parser,shared)
  .settings(
    scalaVersion := "3.1.1",
    scalacOptions ++= Seq("-new-syntax", "-indent")
  )

// Libraries
shared / libraryDependencies := Seq(
  "com.outr" %% "scribe" % "3.8.2",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.0.0",
).map(_.cross(CrossVersion.for3Use2_13))
parser / libraryDependencies := Seq(
  "com.lihaoyi" %% "fastparse" % "2.3.0",
)
root / libraryDependencies := Seq(
  "org.scalatest" %% "scalatest" % "3.2.11" % "test",
  "com.github.scopt" %% "scopt" % "4.0.1",
)

// Scalac options
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature",
  "-Xfatal-warnings"
)
root / scalacOptions ++= Seq(
  /* "-Ywarn-unused", */
  /* "-Ywarn-value-discard", */
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
assembly / assemblyOption := (assembly / assemblyOption ).value.copy(
    prependShellScript = Some(sbtassembly.AssemblyPlugin.defaultShellScript)
)
assembly / assemblyJarName  := "fuse.jar"
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
