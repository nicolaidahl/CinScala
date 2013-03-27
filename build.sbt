name := "cinscala"

organization := "dk.itu"

version := "0.3-SNAPSHOT"

scalaVersion := "2.10.0"

scalacOptions += "-deprecation"

resolvers ++= Seq(
  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "releases"  at "http://oss.sonatype.org/content/repositories/releases",
  "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"
)

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.10.0" % "test"
  //"org.scalaz" %% "scalaz-core" % "7.0-SNAPSHOT"
)

