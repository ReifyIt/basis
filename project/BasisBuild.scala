import sbt._
import Keys._

object BasisBuild extends Build {
  lazy val basis = Project(
    id       = "basis",
    base     = file("."),
    settings = commonSettings ++ Seq(
      modulePath := "."
    )
  )
  
  lazy val basisMemory = Project(
    id       = "basis-memory",
    base     = file("."),
    settings = commonSettings ++ Seq(
      modulePath := "basis/memory"
    )
  )
  
  lazy val basisJSON = Project(
    id           = "basis-json",
    base         = file("."),
    settings = commonSettings ++ Seq(
      modulePath := "basis/json"
    )
  )
  
  lazy val basisAlgebra = Project(
    id           = "basis-algebra",
    base         = file("."),
    settings     = commonSettings ++ Seq(
      modulePath := "basis/algebra"
    )
  )
  
  lazy val commonSettings = Defaults.defaultSettings ++ Seq(
    organization := "com.scalabasis",
    version      := "0.0-SNAPSHOT",
    scalaVersion := "2.10.0-M4",
    scalaSource in Compile <<= (scalaSource in Compile, modulePath)(_ / _),
    scalaSource in Test <<= (scalaSource in Test, modulePath)(_ / _),
    target <<= (target, name)(_ / _),
    scalacOptions ++= Seq("-optimise", "-Xno-forwarders"),
    scalacOptions in (Compile, doc) <++= (version, baseDirectory in LocalProject("basis")) map {
      (version, baseDirectory) =>
        val tagOrBranch = if (version.endsWith("-SNAPSHOT")) "master" else "v" + version
        val docSourceUrl = "https://github.com/scalabasis/basis/tree/" + tagOrBranch + "€{FILE_PATH}.scala"
        Seq("-sourcepath", baseDirectory.getAbsolutePath, "-doc-source-url", docSourceUrl)
    },
    libraryDependencies += "org.scalatest" % "scalatest_2.10.0-M4" % "1.8-SNAPSHOT" % "test",
    resolvers += Resolver.sonatypeRepo("snapshots")
  )
  
  val modulePath = SettingKey[String]("module-path", "the relative path of the module's root package")
}
