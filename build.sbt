scalaVersion := "3.9.0"

lazy val root = rootProject
  .settings(
    name := "table-processor",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.19" % Test
    )
  )
