/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012-2013 Reify It            **
**  |_____/\_____\____/__/\____/      http://basis.reify.it             **
\*                                                                      */

package basis

import sbt._
import sbt.Keys._
import Defaults.defaultSettings

object BasisBuild extends Build {
  lazy val Basis = Project(
    id           = "basis",
    base         = file("."),
    settings     = baseSettings,
    dependencies =
      Seq(BasisCollections,
          BasisConcurrent,
          BasisContainers,
          BasisControl,
          BasisDispatch,
          BasisMath,
          BasisMemory,
          BasisParallel,
          BasisRandom,
          BasisRuntime,
          BasisSequential,
          BasisText,
          BasisUtil),
    aggregate    =
      Seq(BasisCollections,
          BasisConcurrent,
          BasisContainers,
          BasisControl,
          BasisDispatch,
          BasisMath,
          BasisMemory,
          BasisParallel,
          BasisRandom,
          BasisRuntime,
          BasisSequential,
          BasisText,
          BasisUtil)
  )
  
  lazy val BasisCollections = Project(
    id           = "basis-collections",
    base         = file("basis-collections"),
    settings     = commonSettings,
    dependencies =
      Seq(BasisControl,
          BasisUtil)
  )
  
  lazy val BasisConcurrent = Project(
    id           = "basis-concurrent",
    base         = file("basis-concurrent"),
    settings     = commonSettings,
    dependencies =
      Seq(BasisCollections,
          BasisContainers,
          BasisRuntime)
  )
  
  lazy val BasisContainers = Project(
    id           = "basis-containers",
    base         = file("basis-containers"),
    settings     = commonSettings,
    dependencies =
      Seq(BasisCollections,
          BasisMemory,
          BasisRuntime,
          BasisUtil)
  )
  
  lazy val BasisControl = Project(
    id           = "basis-control",
    base         = file("basis-control"),
    settings     = commonSettings,
    dependencies =
      Seq(BasisRuntime,
          BasisUtil)
  )
  
  lazy val BasisDispatch = Project(
    id           = "basis-dispatch",
    base         = file("basis-dispatch"),
    settings     = commonSettings,
    dependencies =
      Seq(BasisConcurrent,
          BasisContainers,
          BasisControl)
  )
  
  lazy val BasisMath = Project(
    id           = "basis-math",
    base         = file("basis-math"),
    settings     = commonSettings
  )
  
  lazy val BasisMemory = Project(
    id           = "basis-memory",
    base         = file("basis-memory"),
    settings     = commonSettings,
    dependencies =
      Seq(BasisRuntime,
          BasisUtil)
  )
  
  lazy val BasisParallel = Project(
    id           = "basis-parallel",
    base         = file("basis-parallel"),
    settings     = commonSettings,
    dependencies =
      Seq(BasisCollections,
          BasisControl,
          BasisDispatch,
          BasisSequential)
  )
  
  lazy val BasisRandom = Project(
    id           = "basis-random",
    base         = file("basis-random"),
    settings     = commonSettings,
    dependencies =
      Seq(BasisCollections,
          BasisContainers,
          BasisUtil)
  )
  
  lazy val BasisRuntime = Project(
    id           = "basis-runtime",
    base         = file("basis-runtime"),
    settings     = commonSettings
  )
  
  lazy val BasisSequential = Project(
    id           = "basis-sequential",
    base         = file("basis-sequential"),
    settings     = commonSettings,
    dependencies =
      Seq(BasisCollections,
          BasisControl,
          BasisUtil)
  )
  
  lazy val BasisText = Project(
    id           = "basis-text",
    base         = file("basis-text"),
    settings     = commonSettings,
    dependencies =
      Seq(BasisCollections,
          BasisUtil)
  )
  
  lazy val BasisUtil = Project(
    id           = "basis-util",
    base         = file("basis-util"),
    settings     = commonSettings
  )
  
  lazy val baseSettings =
    defaultSettings ++
    Unidoc.settings ++
    projectSettings ++
    scalaSettings   ++
    docSettings     ++
    publishSettings
  
  lazy val commonSettings =
    baseSettings    ++
    compileSettings
  
  lazy val projectSettings = Seq(
    version      := "0.1-SNAPSHOT",
    organization := "it.reify",
    description  := "An experimental foundation library for Scala focussed on efficiency and clean design.",
    homepage     := Some(url("http://basis.reify.it")),
    licenses     := Seq("MIT" -> url("http://www.opensource.org/licenses/mit-license.php")),
    resolvers    += Resolver.sonatypeRepo("snapshots")
  )
  
  lazy val scalaSettings = Seq(
    scalaVersion   := "2.10.1-RC2",
    scalacOptions ++= Seq("-language:_", "-Yno-predef")
  )
  
  lazy val compileSettings = Seq(
    scalacOptions in Compile ++= Seq("-optimise", "-Xno-forwarders", "-Ywarn-all"),
    javacOptions  in Compile ++= Seq("-Xlint", "-XDignore.symbol.file"),
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _ % "provided")
  )
  
  lazy val docSettings = Seq(
    scalacOptions in doc <++= (version, baseDirectory in LocalProject("basis")) map { (version, baseDirectory) =>
      val tagOrBranch = if (version.endsWith("-SNAPSHOT")) "master" else "v" + version
      val docSourceUrl = "https://github.com/reifyit/basis/tree/" + tagOrBranch + "€{FILE_PATH}.scala"
      Seq("-groups",
          "-implicits",
          "-implicits-hide:basis.control.MaybeOps,.",
          "-diagrams",
          "-sourcepath", baseDirectory.getAbsolutePath,
          "-doc-source-url", docSourceUrl)
    }
  )
  
  lazy val publishSettings = Seq(
    publishMavenStyle := true,
    publishTo <<= version { version =>
      val nexus = "https://oss.sonatype.org/"
      if (version.endsWith("SNAPSHOT")) Some("snapshots" at nexus + "content/repositories/snapshots")
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
    }
  )
}
