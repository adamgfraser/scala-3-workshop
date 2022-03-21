import sbt.Keys._ 
import sbt._ 

lazy val root = project
  .in(file(""))
  .settings(
    name := "scala-3-workshop",
    version := "0.1.0",
    scalacOptions ++= Seq(
      "-language:postfixOps",
      "-Ykind-projector",
      "-Yexplicit-nulls",
      "-source", "future",
      "-Xfatal-warnings"
    ),
    scalaVersion := "3.1.1"
  )