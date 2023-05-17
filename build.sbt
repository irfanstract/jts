













scalaVersion := "3.2.2"

scalacOptions += "-Yexplicit-nulls"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.9" % Test
)

libraryDependencies ++= Seq(
  "org.ow2.asm" % "asm" % "9.5"
)

Compile / resourceDirectories += (
  baseDirectory.value / "src" / "main" / "resources"
)










