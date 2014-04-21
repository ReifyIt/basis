import sbt._
import sbt.Keys._

object Unidoc {
  val unidocDirectory = SettingKey[File]("unidoc-directory")
  val unidocExclude = SettingKey[Seq[String]]("unidoc-exclude")
  val unidocAllSources = TaskKey[Seq[Seq[File]]]("unidoc-all-sources")
  val unidocSources = TaskKey[Seq[File]]("unidoc-sources")
  val unidocAllClasspaths = TaskKey[Seq[Classpath]]("unidoc-all-classpaths")
  val unidocClasspath = TaskKey[Seq[File]]("unidoc-classpath")
  val unidoc = TaskKey[File]("unidoc", "Generate unified scaladocs for all aggregated projects")

  lazy val settings = Seq(
    unidocDirectory := crossTarget.value / "api",
    unidocExclude := Seq.empty,
    unidocAllSources := unidocAllSourcesTask.value,
    unidocSources := unidocAllSources.value.flatten,
    unidocAllClasspaths := unidocAllClasspathsTask.value,
    unidocClasspath := unidocAllClasspaths.value.flatten.map(_.data).distinct,
    unidoc := unidocTask.value)

  private lazy val unidocAllSourcesTask = Def.settingDyn {
    val excludeProjects = unidocExclude.value.map(LocalProject)
    sources.all(ScopeFilter(inAggregates(thisProjectRef.value) -- inProjects(excludeProjects: _*), inConfigurations(Compile)))
  }

  private lazy val unidocAllClasspathsTask = Def.settingDyn {
    val excludeProjects = unidocExclude.value.map(LocalProject)
    dependencyClasspath.all(ScopeFilter(inAggregates(thisProjectRef.value) -- inProjects(excludeProjects: _*), inConfigurations(Compile)))
  }

  private lazy val unidocTask = Def.task {
    val scaladoc = Doc.scaladoc("main", streams.value.cacheDirectory / "api", compilers.value.scalac)
    val scaladocOptions = (scalacOptions in (Compile, doc)).value ++ Opts.doc.externalAPI((apiMappings in (Compile, doc)).value)
    scaladoc(unidocSources.value, unidocClasspath.value, unidocDirectory.value, scaladocOptions, 100, streams.value.log)
    unidocDirectory.value
  }
}
