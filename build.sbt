lazy val modules = Seq(`basis-core`, `basis-data`, `basis-form`, `basis-math`, `basis-proto`, `basis-stat`, `basis-util`)

lazy val basis = project in file(".") settings (Unidoc.settings ++ rootSettings: _*) dependsOn (modules map (x => x: ClasspathDep[ProjectReference]): _*) aggregate (modules map (x => x: ProjectReference): _*)

lazy val `basis-core` = project in file("core") settings (moduleSettings: _*) dependsOn (`basis-util`)

lazy val `basis-data` = project in file("data") settings (moduleSettings: _*) dependsOn (`basis-core`, `basis-util`)

lazy val `basis-form` = project in file("form") settings (moduleSettings: _*) dependsOn (`basis-core`, `basis-data`, `basis-proto`, `basis-util`)

lazy val `basis-math` = project in file("math") settings (moduleSettings: _*)

lazy val `basis-proto` = project in file("proto") settings (moduleSettings: _*) dependsOn (`basis-core`, `basis-data`, `basis-util`)

lazy val `basis-stat` = project in file("stat") settings (moduleSettings: _*) dependsOn (`basis-core`, `basis-util`)

lazy val `basis-util` = project in file("util") settings (moduleSettings: _*)

lazy val rootSettings = projectSettings ++ docSettings ++ publishSettings

lazy val moduleSettings = rootSettings ++ compileSettings

lazy val projectSettings = Seq(
  version := "0.2.0-SNAPSHOT",
  organization := "it.reify",
  description := "A foundation library for Scala focussed on efficiency and clean design",
  licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  homepage := Some(url("http://basis.reify.it")),
  scalaVersion := "2.11.2",
  scalacOptions ++= Seq("-language:_", "-Yno-predef"))

lazy val compileSettings = Seq(
  scalacOptions ++= Seq("-optimise", "-deprecation", "-Xfuture", "-Xlint", "-Ywarn-adapted-args", "-Ywarn-inaccessible", "-Ywarn-infer-any", "-Ywarn-nullary-override", "-Ywarn-nullary-unit", "-Ywarn-unused", "-Ywarn-unused-import"),
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
    "org.scalatest" %% "scalatest" % "2.2.1" % "test"))

lazy val docSettings = Seq(
  apiURL := Some(url("http://basis.reify.it/api/")),
  scalacOptions in (Compile, doc) ++= {
    val tagOrBranch = if (version.value.endsWith("-SNAPSHOT")) "master" else "v" + version.value
    val docSourceUrl = "https://github.com/reifyit/basis/tree/" + tagOrBranch + "€{FILE_PATH}.scala"
    Seq("-groups",
        "-implicits",
        "-implicits-hide:basis.MaybeToOps,basis.util.ArrowToOps,.",
        "-implicits-show-all",
        "-diagrams",
        "-sourcepath", (baseDirectory in LocalProject("basis")).value.getAbsolutePath,
        "-doc-source-url", docSourceUrl,
        "-Ymacro-expand:none")
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

// Root project settings

libraryDependencies += "org.bouncycastle" % "bcpkix-jdk15on" % "1.51" % "optional"
