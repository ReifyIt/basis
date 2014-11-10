import com.typesafe.sbt.site.SphinxSupport._

site.settings

site.sphinxSupport()

sourceDirectory in Sphinx := sourceDirectory.value / "docs"

sourceDirectory in Test := (sourceDirectory in Sphinx).value

site.addMappingsToSiteDir(mappings in packageDoc in Compile in LocalProject("basis"), "latest/api")

publishArtifact in Compile := false
