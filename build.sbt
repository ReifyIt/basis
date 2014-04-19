description := "A foundation library for Scala focussed on efficiency and clean design"

licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

homepage := Some(url("http://basis.reify.it"))

organization := "it.reify"

version := "0.1-SNAPSHOT"

lazy val `basis-collections` = project in file("collections") dependsOn `basis-util`

lazy val `basis-data` = project in file("data") dependsOn (`basis-collections`, `basis-text`, `basis-util`)

lazy val `basis-form` = project in file("form") dependsOn (`basis-collections`, `basis-data`, `basis-text`, `basis-util`)

lazy val `basis-math` = project in file("math")

lazy val `basis-stat` = project in file("stat") dependsOn (`basis-collections`, `basis-util`)

lazy val `basis-text` = project in file("text") dependsOn (`basis-collections`, `basis-util`)

lazy val `basis-util` = project in file("util")

libraryDependencies in Global ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scalatest" %% "scalatest" % "2.1.3" % "test")

scalaVersion in Global := "2.11.0-RC4"

scalacOptions in Global ++= Seq("-optimise", "-language:_", "-Yno-predef", "-Xfuture", "-Xlint", "-Ywarn-adapted-args", "-Ywarn-inaccessible", "-Ywarn-infer-any", "-Ywarn-nullary-override", "-Ywarn-nullary-unit", "-Ywarn-unused", "-Ywarn-unused-import", "-Ywarn-value-discard")

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
}

pomIncludeRepository := (_ => false)

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (version.value.endsWith("SNAPSHOT")) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

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
