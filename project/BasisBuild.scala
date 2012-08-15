import sbt._
import Keys._

object BasisBuild extends Build {
  lazy val basis = Project(
    id       = "basis",
    base     = file("."),
    settings = commonSettings ++ Seq(
      modulePath := ".",
      libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.10.0-M6" % "provided"
    )
  )
  
  lazy val basisAlgebra = Project(
    id       = "basis-algebra",
    base     = file("."),
    settings = commonSettings ++ Seq(
      modulePath := "basis/algebra"
    )
  )
  
  lazy val basisArithmetic = Project(
    id       = "basis-arithmetic",
    base     = file("."),
    settings = commonSettings ++ Seq(
      modulePath := "basis/arithmetic"
    ),
    dependencies = Seq(basisAlgebra)
  )
  
  lazy val basisCollection = Project(
    id       = "basis-collection",
    base     = file("."),
    settings = commonSettings ++ Seq(
      modulePath := "basis/collection"
    )
  )
  
  lazy val basisContainer = Project(
    id       = "basis-container",
    base     = file("."),
    settings = commonSettings ++ Seq(
      modulePath := "basis/container"
    ),
    dependencies = Seq(basisCollection, basisMemory)
  )
  
  lazy val basisEncoding = Project(
    id       = "basis-encoding",
    base     = file("."),
    settings = commonSettings ++ Seq(
      modulePath := "basis/encoding"
    ),
    dependencies = Seq(basisCollection)
  )
  
  lazy val basisJSON = Project(
    id       = "basis-json",
    base     = file("."),
    settings = commonSettings ++ Seq(
      modulePath := "basis/json",
      libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.10.0-M6" % "provided"
    )
  )
  
  lazy val basisMemory = Project(
    id       = "basis-memory",
    base     = file("."),
    settings = commonSettings ++ Seq(
      modulePath := "basis/memory"
    )
  )
  
  lazy val commonSettings = Defaults.defaultSettings ++ projectSettings ++ compileSettings ++ publishSettings
  
  lazy val projectSettings = Seq(
    version      := "0.0-SNAPSHOT",
    organization := "com.scalabasis",
    description  := "A library of scalable building blocks",
    homepage     := Some(url("http://www.scalabasis.com/")),
    licenses     := Seq("MIT" -> url("http://www.opensource.org/licenses/mit-license.php"))
  )
  
  lazy val compileSettings = Seq(
    scalaVersion := "2.10.0-M6",
    scalaSource in Compile <<= (scalaSource in Compile, modulePath)(_ / _),
    scalaSource in Test <<= (scalaSource in Test, modulePath)(_ / _),
    scalacOptions ++= Seq("-optimise", "-Xno-forwarders", "-Yinline-warnings"),
    scalacOptions in (Compile, doc) <++= (version, baseDirectory in LocalProject("basis")) map {
      (version, baseDirectory) =>
        val tagOrBranch = if (version.endsWith("-SNAPSHOT")) "master" else "v" + version
        val docSourceUrl = "https://github.com/scalabasis/basis/tree/" + tagOrBranch + "€{FILE_PATH}.scala"
        Seq("-implicits", "-diagrams", "-sourcepath", baseDirectory.getAbsolutePath, "-doc-source-url", docSourceUrl)
    },
    target <<= (target, name)(_ / _),
    resolvers += Resolver.sonatypeRepo("snapshots")
    //libraryDependencies += "org.scalatest" % "scalatest_2.10.0-M6" % "1.8-SNAPSHOT" % "test"
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
  
  val modulePath = SettingKey[String]("module-path", "the relative path of the module's root package")
}
