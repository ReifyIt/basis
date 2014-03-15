lazy val quasiSettings: Seq[Setting[_]] = moduleSettings :+ {
  libraryDependencies <++= (scalaVersion)(_ match {
    case "2.10.3" => List("org.scalamacros" % "quasiquotes_2.10.3" % "2.0.0-M3", "org.scalamacros" % "paradise_2.10.3" % "2.0.0-M3" % "plugin")
    case _        => Nil
  })
}

scalaVersion in Global := "2.10.3"

scalacOptions in Global ++= Seq("-language:experimental.macros", "-Yno-predef")

retrieveManaged := true

lazy val subprojects = List(`basis-collections`, `basis-data`, `basis-form`, `basis-math`, `basis-stat`, `basis-text`, `basis-util`)

lazy val basis = (
  project in file(".")
     settings (packageSettings ++ unidocSettings: _*)
    dependsOn (subprojects map (x => x: ClasspathDep[ProjectReference]): _*)
    aggregate (subprojects map (x => x: ProjectReference): _*)
)

lazy val `basis-collections` = project in file("collections") settings (quasiSettings: _*) dependsOn `basis-util`

lazy val `basis-data` = project in file("data") settings (quasiSettings: _*) dependsOn (`basis-collections`, `basis-text`, `basis-util`)

lazy val `basis-form` = project in file("form") settings (quasiSettings: _*) dependsOn (`basis-collections`, `basis-data`, `basis-text`, `basis-util`)

lazy val `basis-math` = project in file("math") settings (moduleSettings: _*)

lazy val `basis-stat` = project in file("stat") settings (moduleSettings: _*) dependsOn (`basis-collections`, `basis-util`)

lazy val `basis-text` = project in file("text") settings (moduleSettings: _*) dependsOn (`basis-collections`, `basis-util`)

lazy val `basis-util` = project in file("util") settings (quasiSettings: _*)

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
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "org.scalatest" %% "scalatest" % "2.1.0" % "test"
  )
)

lazy val docSettings = Seq(
  apiURL := Some(url("http://basis.reify.it/api/")),
  scalacOptions in (Compile, doc) ++= {
    val tagOrBranch = if (version.value.endsWith("-SNAPSHOT")) "master" else "v" + version.value
    val docSourceUrl = "https://github.com/reifyit/basis/tree/" + tagOrBranch + "€{FILE_PATH}.scala"
    Seq("-groups",
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
