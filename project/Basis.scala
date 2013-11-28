//      ____              ___
//     / __ | ___  ____  /__/___      A library of building blocks
//    / __  / __ |/ ___|/  / ___|
//   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2014 Reify It
//  |_____/\_____\____/__/\____/      http://basis.reify.it

package basis

import sbt._
import sbt.Keys._
import sbtunidoc.Plugin._

object Basis extends Build {
  lazy val Basis = Project(
    id = "basis",
    base = file("."),
    settings = packageSettings ++ unidocSettings,
    dependencies =
      Seq(BasisCollections,
          BasisForm,
          BasisMath,
          BasisMemory,
          BasisStat,
          BasisText,
          BasisUtil),
    aggregate =
      Seq(BasisCollections,
          BasisForm,
          BasisMath,
          BasisMemory,
          BasisStat,
          BasisText,
          BasisUtil))

  lazy val BasisCollections = Project(
    id = "basis-collections",
    base = file("basis-collections"),
    settings = moduleSettings,
    dependencies = Seq(BasisUtil))

  lazy val BasisForm = Project(
    id = "basis-form",
    base = file("basis-form"),
    settings = moduleSettings,
    dependencies =
      Seq(BasisCollections,
          BasisMemory,
          BasisText,
          BasisUtil))

  lazy val BasisMath = Project(
    id = "basis-math",
    base = file("basis-math"),
    settings = moduleSettings)

  lazy val BasisMemory = Project(
    id = "basis-memory",
    base = file("basis-memory"),
    settings = moduleSettings,
    dependencies =
      Seq(BasisCollections,
          BasisUtil))

  lazy val BasisStat = Project(
    id = "basis-stat",
    base = file("basis-stat"),
    settings = moduleSettings,
    dependencies =
      Seq(BasisCollections,
          BasisUtil))

  lazy val BasisText = Project(
    id = "basis-text",
    base = file("basis-text"),
    settings = moduleSettings,
    dependencies =
      Seq(BasisCollections,
          BasisUtil))

  lazy val BasisUtil = Project(
    id = "basis-util",
    base = file("basis-util"),
    settings = moduleSettings)

  lazy val packageSettings =
    Defaults.defaultSettings ++
    projectSettings ++
    scalaSettings ++
    docSettings ++
    publishSettings

  lazy val moduleSettings =
    packageSettings ++
    compileSettings

  lazy val projectSettings = Seq(
    version := "0.1-SNAPSHOT",
    organization := "it.reify",
    description := "A foundation library for Scala focussed on efficiency and clean design",
    homepage := Some(url("http://basis.reify.it")),
    licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
    resolvers += Resolver.sonatypeRepo("snapshots"))

  lazy val scalaSettings = Seq(
    scalaVersion := "2.10.3",
    scalacOptions ++= Seq("-language:_", "-Yno-predef"))

  lazy val compileSettings = Seq(
    scalacOptions in Compile ++= Seq("-optimise", "-Xno-forwarders", "-Ywarn-all"),
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
      "org.scalatest" %% "scalatest" % "1.9.2" % "test"))

  lazy val docSettings = Seq(
    apiURL := Some(url("http://basis.reify.it/api/")),
    scalacOptions in (Compile, doc) ++= {
      val tagOrBranch = if (version.value.endsWith("-SNAPSHOT")) "master" else "v" + version.value
      val docSourceUrl = "https://github.com/reifyit/basis/tree/" + tagOrBranch + "€{FILE_PATH}.scala"
      Seq("-groups",
          "-implicits",
          "-implicits-hide:basis.util.ArrowOps,basis.util.MaybeOps,.",
          "-implicits-show-all",
          "-diagrams",
          "-sourcepath", (baseDirectory in LocalProject("basis")).value.getAbsolutePath,
          "-doc-source-url", docSourceUrl)
    })

  lazy val publishSettings = Seq(
    publishMavenStyle := true,
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (version.value.endsWith("SNAPSHOT")) Some("snapshots" at nexus + "content/repositories/snapshots")
      else Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    pomIncludeRepository := (_ => false),
    pomExtra := {
      <scm>
        <url>git@github.com:reifyit/basis.git</url>
        <connection>scm:git:git@github.com:reifyit/basis.git</connection>
      </scm>
      <developers>
        <developer>
          <id>c9r</id>
          <name>Chris Sachs</name>
          <email>chris@reify.it</email>
        </developer>
      </developers>
    })
}
