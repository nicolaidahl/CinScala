name := "Scala C"

version := "0.01"

scalaVersion := "2.9.2"

resolvers ++= Seq(
  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "releases"  at "http://oss.sonatype.org/content/repositories/releases",
  "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"
)

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.9" % "test"
  //"org.scalaz" %% "scalaz-core" % "7.0-SNAPSHOT"
)
