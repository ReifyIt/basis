val scalaVer    = "2.10.3"
val paradiseVer = "2.0.0-M3"

scalaVersion in Global := scalaVer

scalacOptions in Global ++= Seq("-language:_", "-Yno-predef")

retrieveManaged := true

lazy val subprojects = List(collections, form, math, memory, stat, text, util)

lazy val basis = (
  project in file(".")
     settings (packageSettings ++ unidocSettings: _*)
    dependsOn (subprojects map (x => x: ClasspathDep[ProjectReference]): _*)
    aggregate (subprojects map (x => x: ProjectReference): _*)
)

lazy val math, util = project settings (moduleSettings: _*)

lazy val collections = project settings (moduleSettings: _*) dependsOn util settings (
  libraryDependencies += "org.scalamacros" % s"quasiquotes_$scalaVer" % paradiseVer,
  addCompilerPlugin("org.scalamacros" % s"paradise_$scalaVer" % paradiseVer)
)

lazy val form = project settings (moduleSettings: _*) dependsOn (collections, memory, text, util)

lazy val memory, stat, text = project settings (moduleSettings: _*) dependsOn (collections, util)

lazy val packageSettings = (
     Defaults.defaultSettings
  ++ projectSettings
  ++ docSettings
  ++ publishSettings
)

lazy val moduleSettings = packageSettings ++ compileSettings

lazy val projectSettings = Seq(
  version := "0.1-SNAPSHOT",
  organization := "it.reify",
  description := "A foundation library for Scala focussed on efficiency and clean design",
  homepage := Some(url("http://basis.reify.it")),
  licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))
)

lazy val compileSettings = Seq(
  scalacOptions in Compile ++= Seq("-optimise", "-Xno-forwarders", "-Ywarn-all"),
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVer,
    "org.scalatest" %% "scalatest" % "1.9.2" % "test"
  )
)

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
