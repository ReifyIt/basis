/*      ____              ___                                           *\
**     / __ | ___  ____  /__/___      A library of building blocks      **
**    / __  / __ |/ ___|/  / ___|                                       **
**   / /_/ / /_/ /\__ \/  /\__ \      (c) 2012 Chris Sachs              **
**  |_____/\_____\____/__/\____/      http://www.scalabasis.com/        **
\*                                                                      */

package basis

import sbt._
import sbt.Keys._

object BasisBuild extends Build {
  lazy val all = Project(
    id           = "all",
    base         = file("."),
    settings     = commonSettings,
    aggregate    = Seq(
      Basis,
      BasisCollection,
      BasisContainer,
      BasisData,
      BasisText,
      BasisUtil)
  )
  
  lazy val Basis = Project(
    id           = "basis",
    base         = file("basis"),
    settings     = commonSettings
  )
  
  lazy val BasisCollection = Project(
    id           = "basis-collection",
    base         = file("basis-collection"),
    settings     = commonSettings ++ Seq(
      libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.0-SNAPSHOT" % "provided"
    ),
    dependencies = Seq(Basis)
  )
  
  lazy val BasisContainer = Project(
    id           = "basis-container",
    base         = file("basis-container"),
    settings     = commonSettings ++ Seq(
      libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.0-SNAPSHOT" % "provided"
    ),
    dependencies = Seq(Basis, BasisData)
  )
  
  lazy val BasisData = Project(
    id           = "basis-data",
    base         = file("basis-data"),
    settings     = commonSettings ++ Seq(
      libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.0-SNAPSHOT" % "provided"
    )
  )
  
  lazy val BasisText = Project(
    id           = "basis-text",
    base         = file("basis-text"),
    settings     = commonSettings ++ Seq(
      libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.0-SNAPSHOT" % "provided"
    ),
    dependencies = Seq(Basis)
  )
  
  lazy val BasisUtil = Project(
    id           = "basis-util",
    base         = file("basis-util"),
    settings     = commonSettings ++ Seq(
      libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.0-SNAPSHOT" % "provided"
    ),
    dependencies = Seq(Basis)
  )
  
  lazy val commonSettings =
    Defaults.defaultSettings ++
    Unidoc.settings          ++
    projectSettings          ++
    compileSettings          ++
    publishSettings
  
  lazy val projectSettings = Seq(
    version      := "0.0-SNAPSHOT",
    organization := "com.scalabasis",
    description  := "A library of building blocks",
    homepage     := Some(url("http://www.scalabasis.com/")),
    licenses     := Seq("MIT" -> url("http://www.opensource.org/licenses/mit-license.php"))
  )
  
  lazy val compileSettings = Seq(
    scalaVersion := "2.11.0-SNAPSHOT",
    scalacOptions in Compile ++= Seq("-optimise", "-Xno-forwarders", "-Yno-imports", "-Yinline-warnings", "-Ywarn-all"),
    scalacOptions in doc <++= (version, baseDirectory in LocalProject("basis")) map {
      (version, baseDirectory) =>
        val tagOrBranch = if (version.endsWith("-SNAPSHOT")) "master" else "v" + version
        val docSourceUrl = "https://github.com/scalabasis/basis/tree/" + tagOrBranch + "€{FILE_PATH}.scala"
        Seq("-Yno-imports", "-implicits", "-diagrams", "-sourcepath",
            baseDirectory.getAbsolutePath, "-doc-source-url", docSourceUrl)
    },
    resolvers += Resolver.sonatypeRepo("snapshots")
    //libraryDependencies += "org.scalatest" % "scalatest_2.10.0-M7" % "1.8-SNAPSHOT" % "test"
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
        <url>git@github.com:scalabasis/basis.git</url>
        <connection>scm:git:git@github.com:scalabasis/basis.git</connection>
      </scm>
      <developers>
        <developer>
          <id>c9r</id>
          <name>Chris Sachs</name>
          <email>chris@scalabasis.com</email>
        </developer>
      </developers>
    }
  )
}
