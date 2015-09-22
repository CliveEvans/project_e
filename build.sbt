name := "project_euler"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "3.6.4" % "test",
  "org.specs2" %% "specs2-matcher-extra" % "3.6.4" % "test",
  "org.specs2" %% "specs2-mock" % "3.6.4" % "test",
  "org.specs2" %% "specs2-scalacheck" % "3.6.4" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")