import sbt._

object Dependencies {
  lazy val logging: Seq[ModuleID] = Seq(
    "org.typelevel" %% "log4cats-slf4j" % "2.5.0"
  )

  lazy val testing: Seq[ModuleID] = Seq(
    "org.scalactic" %% "scalactic" % "3.2.15" % Test,
    "org.scalatest" %% "scalatest" % "3.2.15" % Test
  )
}
