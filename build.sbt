ThisBuild / scalaVersion := "2.13.10"
ThisBuild / version := "0.1.0"
ThisBuild / organization := "edu.duke.cs.apex"

val chiselVersion = "3.5.5"

lazy val composer = (project in file(".")).enablePlugins(BuildInfoPlugin).settings(
  name := "composer-hardware",
  buildInfoPackage := "composer",
  version := "0.1.0",
  libraryDependencies ++= Seq(
    "edu.berkeley.cs" %% "chisel3" % "3.5.5",
    "edu.duke.cs.apex" %% "fpnew-wrapper" % "0.1.0",
    "edu.duke.cs.apex" %% "rocketchip-composer-fork" % "0.1.1"
  ),
  resolvers += "reposilite-repository-releases" at "http://10.236.187.28:8080/releases",
  publishTo := Some(("reposilite-repository" at "http://10.236.187.28:8080/releases/").withAllowInsecureProtocol(true)),
  credentials += Credentials(Path.userHome / ".sbt" / ".credentials"),
  addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full)
)

