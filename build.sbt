ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.9"

lazy val root = (project in file("."))
  .settings(
    name := "lima-ui"
  )

libraryDependencies ++= Seq(
  "org.openjfx" % "javafx-controls" % "17.0.2",
)

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.14"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.14" % "test"

val tethysVersion = "0.26.0"
libraryDependencies ++= Seq(
  "com.tethys-json" %% "tethys-core" % tethysVersion,
  "com.tethys-json" %% "tethys-jackson" % tethysVersion,
  "com.tethys-json" %% "tethys-derivation" % tethysVersion
)

libraryDependencies += "com.tethys-json" %% "tethys-circe" % tethysVersion

libraryDependencies += "com.softwaremill.magnolia1_2" %% "magnolia" % "1.1.2"

resourceDirectory in Compile := baseDirectory.value / "src" / "main" / "resources"