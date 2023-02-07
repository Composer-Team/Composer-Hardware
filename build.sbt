ThisBuild / scalaVersion := "2.13.10"
ThisBuild / version := "0.1.0"
ThisBuild / organization := "edu.duke.cs.apex"

val chiselVersion = "3.5.5"

lazy val fpnew_wrapper = project in file("fpnew-wrapper")
lazy val rocketchip = project in file("rocket-chip")
lazy val hardfloat = project in file("berkeley-hardfloat")

lazy val composer = (project in file("composer")).enablePlugins(BuildInfoPlugin).settings(
  name := "composer_tools",
  buildInfoPackage := "composer",
  libraryDependencies ++= Seq(
    "edu.berkeley.cs" %% "chisel3" % "3.5.5",
    "edu.berkeley.cs" %% "dsptools" % "1.5.5"
  ),
  addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full),
).dependsOn(rocketchip).dependsOn(hardfloat).dependsOn(fpnew_wrapper)

lazy val root = (project in file(".")).settings(
  name := "Composer",
  libraryDependencies ++= Seq(
    "edu.berkeley.cs" %% "chisel3" % chiselVersion,
    "org.json4s" %% "json4s-jackson" % "3.6.6",
  ),
  addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full),
  serverConnectionType := ConnectionType.Tcp
).dependsOn(composer)