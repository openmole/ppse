val scala3Version = "3.0.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "result",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.9.1" cross CrossVersion.for3Use2_13,
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
