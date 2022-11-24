ThisBuild / scalaVersion := "2.13.10"
ThisBuild / version := "0.1.0"
ThisBuild / organization := "%ORGANIZATION%"

val chiselVersion = "3.5.5"

lazy val rocketchip = project in file("rocket-chip")
lazy val composer = (project in file("composer")).enablePlugins(BuildInfoPlugin).settings(
  name := "composer_tools",
  buildInfoPackage := "composer",
  libraryDependencies ++= Seq(
    "edu.berkeley.cs" %% "chisel3" % "3.5.5",
  ),
  addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full),
).dependsOn(rocketchip)

lazy val root = (project in file(".")).settings(
  name := "Composer",
  libraryDependencies ++= Seq(
    "edu.berkeley.cs" %% "chisel3" % chiselVersion,
    "org.json4s" %% "json4s-jackson" % "3.6.6",
  ),
  addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full),
  serverConnectionType := ConnectionType.Tcp
).dependsOn(composer)