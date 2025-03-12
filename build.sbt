
val chiselVersion = "3.5.6"

ThisBuild / scalaVersion := "2.13.10"

lazy val beethoven =(project in file(".")).settings(
  name := "beethoven-hardware",
  version := "0.0.12",
  organization := "edu.duke.cs.apex",
  libraryDependencies ++= Seq(
    "edu.berkeley.cs" %% "chisel3" % chiselVersion,
    "edu.duke.cs.apex" %% "rocketchip-rocketchip-fork" % "0.1.12",
    "org.scalatra.scalate" %% "scalate-core" % "1.9.6",
    "org.slf4j" % "slf4j-api" % "2.0.9",
//    "org.slf4j" % "slf4j-nop" % "2.0.9",
    "edu.berkeley.cs" %% "chiseltest" % "0.5.2"
  ),
  resolvers += ("reposilite-repository-releases" at "http://54.165.244.214:8080/releases").withAllowInsecureProtocol(true),
  publishTo := Some(("reposilite-repository" at "http://54.165.244.214:8080/releases/").withAllowInsecureProtocol(true)),
  credentials += Credentials(Path.userHome / ".sbt" / ".credentials"),
  addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full)
)
