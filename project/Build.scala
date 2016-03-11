import sbt._
import sbt.Keys._

object MyBuild extends Build {

  // Dependencies

  val spireVersion = "0.11.0"

  lazy val shapeless = "com.chuusai" %% "shapeless" % "2.2.5"
  lazy val spire = "org.spire-math" %% "spire" % spireVersion
  lazy val spireLaws = "org.spire-math" %% "spire-laws" % spireVersion

  lazy val metalCore = "org.scala-metal" %% "metal-core" % "0.1.1"
  lazy val metalLibrary = "org.scala-metal" %% "metal-library" % "0.1.1"

  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.0-M7"
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.12.4"
  lazy val machinist = "org.typelevel" %% "machinist" % "0.4.1"
  lazy val discipline = "org.typelevel" %% "discipline" % "0.4"
  lazy val scalaMeter = "com.storm-enroute" %% "scalameter" % "0.6"
  lazy val scalinLib = "net.alasc" %% "scalin-core" % "0.11.0.1"

  lazy val noPublish = Seq(
    publish := (),
    publishLocal := (),
    publishArtifact := false
  )

  // Settings

  override lazy val settings = super.settings ++ Seq(
    organization := "net.alasc",

    scalaVersion := "2.11.7",

    licenses := Seq("BSD-style" -> url("http://opensource.org/licenses/MIT")),
    homepage := Some(url("https://github.com/denisrosset/alasc")),

    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      machinist,
      metalCore,
      metalLibrary
    ),

    scalacOptions ++= Seq(
      //"-no-specialization", // use this to build non-specialized jars
//      "-Yinline-warnings",
      "-optimize",
      "-deprecation",
      "-unchecked",
      "-language:experimental.macros",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-feature"
    ),

    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    resolvers += "bintray/non" at "https://dl.bintray.com/non/maven",
    resolvers += "bintray/denisrosset/metal" at "https://dl.bintray.com/denisrosset/metal",
    resolvers += "bintray/denisrosset/scalin" at "https://dl.bintray.com/denisrosset/scalin",

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
    aggregate(core, laws, tests, scalin).
    settings(alascSettings: _*)

  lazy val alascSettings = Seq(
    name := "alasc"
  ) ++ noPublish

  // Core

  lazy val core = Project("alasc-core", file("core")).
    settings(coreSettings: _*)

  lazy val coreSettings = Seq(
    name := "alasc",
    libraryDependencies ++= Seq(
      shapeless,
      spire,
      metalCore,
      metalLibrary,
      scalaCheck % "test",
      scalaTest % "test"
    )
  )

  // Scalin binding

  lazy val scalin = Project("alasc-scalin", file("scalin"))
    .settings(scalinSettings: _*)
    .dependsOn(core)

  lazy val scalinSettings = Seq(
    name := "alasc-scalin",
    libraryDependencies ++= Seq(
      discipline,
      scalinLib
    )
  )

  // Law checks

  lazy val laws = Project("alasc-laws", file("laws")).
    settings(lawsSettings: _*).
    dependsOn(core, scalin)

  lazy val lawsSettings = Seq(
    name := "alasc-laws",
    libraryDependencies ++= Seq(
      discipline,
      scalinLib,
      scalaCheck,
      scalaTest,
      spireLaws
    )
  )

  // Tests

  lazy val tests = Project("alasc-tests", file("tests")).
    settings(testsSettings: _*).
    dependsOn(core, scalin, laws)

  lazy val testsSettings = Seq(
    name := "alasc-tests",
    libraryDependencies ++= Seq(
      scalinLib,
      scalaTest % "test",
      spireLaws
    )
  ) ++ noPublish

}
