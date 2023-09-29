ThisBuild / scalaVersion := "2.13.10"
ThisBuild / version := "0.1.42"
ThisBuild / organization := "edu.duke.cs.apex"

val chiselVersion = "3.5.5"

lazy val composer = (project in file(".")).settings(
  name := "composer-hardware",
  libraryDependencies ++= Seq(
    "edu.berkeley.cs" %% "chisel3" % "3.5.5",
    "edu.duke.cs.apex" %% "rocketchip-rocketchip-fork" % "0.1.14",
    "org.scalatra.scalate" %% "scalate-core" % "1.9.8"
  ),
  resolvers += ("reposilite-repository-releases" at "http://oak:8080/releases").withAllowInsecureProtocol(true),
  publishTo := Some(("reposilite-repository" at "http://oak:8080/releases/").withAllowInsecureProtocol(true)),
  credentials += Credentials(Path.userHome / ".sbt" / ".credentials"),
  addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full)
)
