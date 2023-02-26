val scala3Version = "3.2.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "sudoku",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++=
      Dependencies.logging ++
        Dependencies.testing ++ Seq(
      "org.apache.commons" % "commons-lang3" % "3.12.0"
    )
  )

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalacOptions ++= Seq(
  "-explain"
)
