lazy val `basis-collections` = project in file("collections") settings (moduleSettings: _*) dependsOn `basis-util`

lazy val `basis-data` = project in file("data") settings (moduleSettings: _*) dependsOn (`basis-collections`, `basis-text`, `basis-util`)

lazy val `basis-form` = project in file("form") settings (moduleSettings: _*) dependsOn (`basis-collections`, `basis-data`, `basis-text`, `basis-util`)

lazy val `basis-math` = project in file("math") settings (moduleSettings: _*)

lazy val `basis-stat` = project in file("stat") settings (moduleSettings: _*) dependsOn (`basis-collections`, `basis-util`)

lazy val `basis-text` = project in file("text") settings (moduleSettings: _*) dependsOn (`basis-collections`, `basis-util`)

lazy val `basis-util` = project in file("util") settings (moduleSettings: _*)

lazy val moduleSettings = projectSettings ++ compileSettings ++ docSettings ++ publishSettings

lazy val projectSettings = Seq(
  version := "0.1-SNAPSHOT",
  organization := "it.reify",
  description := "A foundation library for Scala focussed on efficiency and clean design",
  licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  homepage := Some(url("http://basis.reify.it")))

lazy val compileSettings = Seq(
  scalaVersion := "2.11.0-RC4",
  scalacOptions ++= Seq("-optimise", "-language:_", "-Yno-predef", "-Xfuture", "-Xlint", "-Ywarn-adapted-args", "-Ywarn-inaccessible", "-Ywarn-infer-any", "-Ywarn-nullary-override", "-Ywarn-nullary-unit", "-Ywarn-unused", "-Ywarn-unused-import", "-Ywarn-value-discard"),
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.scalatest" %% "scalatest" % "2.1.3" % "test"))

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
        "-doc-source-url", docSourceUrl,
        "-Ymacro-no-expand")
  })

lazy val publishSettings = Seq(
  pomIncludeRepository := (_ => false),
  publishMavenStyle := true,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (version.value.endsWith("SNAPSHOT")) Some("snapshots" at nexus + "content/repositories/snapshots")
    else Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
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
