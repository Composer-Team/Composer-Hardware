ThisBuild / scalaVersion := "2.12.15"
ThisBuild / version := "0.1.0"
val composerBuildKey: SettingKey[String] = settingKey[String](
  "In case there are multiple Composer builds on a system, this key allows the software" +
    " stack to distinguish configurations.")
ThisBuild / composerBuildKey := "ComposerTemplate"
ThisBuild / organization := "%ORGANIZATION%"

addSbtPlugin("org.xerial.sbt" % "sbt-pack" % "0.14")
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "1.2.0")

lazy val rocketchip = project in file("rocket-chip")
lazy val composer = (project in file("composer")).enablePlugins(BuildInfoPlugin).settings(
  name := "composer_tools",
  buildInfoKeys := Seq[BuildInfoKey](ThisBuild / composerBuildKey),
  buildInfoPackage := "composer",
  libraryDependencies ++= Seq(
    "edu.berkeley.cs" %% "chisel3" % "3.5.4",
    "edu.berkeley.cs" %% "chiseltest" % "0.5.4" % "test"
  ),
  scalacOptions ++= Seq(
    "-Xsource:2.11",
    "-language:reflectiveCalls",
    "-deprecation",
    "-feature",
    "-Xcheckinit"
  ),
  addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % "3.5.2" cross CrossVersion.full),
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)
).dependsOn(rocketchip)

lazy val root = (project in file(".")).settings(
  name := "Composer",
  libraryDependencies ++= Seq(
    "edu.berkeley.cs" %% "chisel3" % "3.5.4",
    "edu.berkeley.cs" %% "chiseltest" % "0.5.4" % "test",
    "org.json4s" %% "json4s-jackson" % "3.6.1",
    "edu.berkeley.cs" %% "dsptools" % "1.5.4"
  ),
  scalacOptions ++= Seq(
    "-Xsource:2.11",
    "-language:reflectiveCalls",
    "-deprecation",
    "-feature",
    "-Xcheckinit"
  ),
  assembly / test := {},
  assembly / assemblyJarName := "rocketchip.jar",
  assembly / assemblyOutputPath := baseDirectory.value / "rocketchip.jar",
  addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % "3.5.2" cross CrossVersion.full),
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full),
  serverConnectionType := ConnectionType.Tcp
).dependsOn(composer)

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs@_*) => MergeStrategy.discard
  case x => MergeStrategy.first
}
