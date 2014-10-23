organization := "net.alasc"

name := "Alasc"

version := "0.96-SNAPSHOT"

scalaVersion := "2.11.2"

resolvers ++= Seq(
  "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases",
  "bintray/non" at "http://dl.bintray.com/non/maven"
)

libraryDependencies ++= Seq(
  "org.spire-math" %% "debox" % "0.7.0-SNAPSHOT",
  "org.scalacheck" %% "scalacheck" % "1.11.4" % "test",
  "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test",
//  "com.storm-enroute" %% "scalameter" % "0.6" % "test",
  "org.spire-math" %% "spire" % "0.8.3-SNAPSHOT"
)

scalacOptions ++= Seq("-unchecked", "-feature", "-deprecation") 

//testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

//parallelExecution in Test := false
