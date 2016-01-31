import sbt._

object RatNestBuild extends Build {
  val Name = "RatNest"
  val utils =
    RootProject(uri("https://github.com/electricmind/utils.git#dev"))

  override lazy val settings = super.settings

  lazy val root = Project(Name,
    base = file("."),
    settings = Project.defaultSettings
  ).dependsOn(utils)
}
