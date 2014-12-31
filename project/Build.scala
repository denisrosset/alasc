import sbt._
import sbt.Keys._

object MyBuild extends Build {

  // Dependencies

  lazy val debox = "org.spire-math" %% "debox" % "0.7.0-SNAPSHOT"
  lazy val spire = "org.spire-math" %% "spire" % "0.9.1-SNAPSHOT"
  lazy val spireScalaCheckBindings = "org.spire-math" %% "spire-scalacheck-binding" % "0.9.1-SNAPSHOT"
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "2.2.1"
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.11.6"
  lazy val machinist = "org.typelevel" %% "machinist" % "0.3.0"
  lazy val discipline = "org.typelevel" %% "discipline" % "0.2.1"
  lazy val scalaMeter = "com.storm-enroute" %% "scalameter" % "0.6"

  lazy val noPublish = Seq(
    publish := (),
    publishLocal := (),
    publishArtifact := false
  )

  // Settings

  override lazy val settings = super.settings ++ Seq(
    organization := "net.alasc",

    scalaVersion := "2.11.4",

    licenses := Seq("BSD-style" -> url("http://opensource.org/licenses/MIT")),
    homepage := Some(url("https://github.com/denisrosset/alasc")),

    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      machinist
    ),

    scalacOptions ++= Seq(
      //"-no-specialization", // use this to build non-specialized jars
      "-Yinline-warnings",
      "-deprecation",
      "-unchecked",
      "-optimize",
      "-language:experimental.macros",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-feature"
    ),

    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    resolvers += "bintray/non" at "http://dl.bintray.com/non/maven",

    // re-enable to check imports, or once scala's REPL manages to not
    // be useless under -Ywarn-unused-import.

    // scalacOptions in Compile := {
    //   CrossVersion.partialVersion(scalaVersion.value) match {
    //     case Some((2, 10)) =>
    //       scalacOptions.value
    //     case Some((2, n)) if n >= 11 =>
    //       scalacOptions.value ++ Seq("-Ywarn-unused-import")
    //   }
    // },

    libraryDependencies := {
      CrossVersion.partialVersion(scalaVersion.value) match {
        // if scala 2.11+ is used, quasiquotes are merged into scala-reflect
        case Some((2, scalaMajor)) if scalaMajor >= 11 =>
          libraryDependencies.value

        // in Scala 2.10, quasiquotes are provided by macro-paradise
        case Some((2, 10)) =>
          libraryDependencies.value ++ Seq(
            compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full),
            "org.scalamacros" %% "quasiquotes" % "2.0.1")
      }
    }
  )

  // Main

  lazy val alasc = Project("alasc", file(".")).
    aggregate(core, scalacheckBinding, tests, benchmark).
    settings(alascSettings: _*)

  lazy val alascSettings = Seq(
    name := "alasc-aggregate"
  ) ++ noPublish

  // Core

  lazy val core = Project("core", file("core")).
    settings(coreSettings: _*)

  lazy val coreSettings = Seq(
    name := "alasc",
    libraryDependencies ++= Seq(
      spire,
      debox,
      scalaCheck % "test",
      scalaTest % "test"
    )
  )

  // Scalacheck binding

  lazy val scalacheckBinding = Project("scalacheck-binding", file("scalacheck-binding")).
    settings(scalacheckSettings: _*).
    dependsOn(core)

  lazy val scalacheckSettings = Seq(
    name := "alasc-scalacheck-binding",
    libraryDependencies ++= Seq(
      discipline,
      scalaCheck,
      spireScalaCheckBindings
    )
  )

  // Tests

  lazy val tests = Project("tests", file("tests")).
    settings(testsSettings: _*).
    dependsOn(core, scalacheckBinding)

  lazy val testsSettings = Seq(
    name := "alasc-tests",
    libraryDependencies ++= Seq(
      scalaTest % "test",
      spireScalaCheckBindings
    )
  ) ++ noPublish


  // Benchmark

  lazy val benchmark: Project = Project("benchmark", file("benchmark")).
    settings(benchmarkSettings: _*).
    dependsOn(core)

  lazy val benchmarkSettings = Seq(
    name := "alasc-benchmark",

    // raise memory limits here if necessary
    // TODO: this doesn't seem to be working with caliper at the moment :(
  
    javaOptions in run += "-Xmx4G",

    libraryDependencies ++= Seq(
      scalaMeter
    ),

    // enable forking in run
    fork in run := true
  ) ++ noPublish

}
