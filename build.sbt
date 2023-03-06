ThisBuild / scalaVersion := "2.13.10"
ThisBuild / version := "0.1.8"
ThisBuild / organization := "edu.duke.cs.apex"

val chiselVersion = "3.5.5"

lazy val composer = (project in file(".")).enablePlugins(BuildInfoPlugin).settings(
  name := "composer-hardware",
  buildInfoPackage := "composer",
  libraryDependencies ++= Seq(
    "edu.berkeley.cs" %% "chisel3" % "3.5.5",
    "edu.duke.cs.apex" %% "rocketchip-composer-fork" % "0.1.2"
  ),
  resolvers += "reposilite-repository-releases" at "http://oak.cs.duke.edu:8080/releases",
  publishTo := Some(("reposilite-repository" at "http://oak.cs.duke.edu:8080/releases/").withAllowInsecureProtocol(true)),
  credentials += Credentials(Path.userHome / ".sbt" / ".credentials"),
  addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full)
)

