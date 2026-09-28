name := "ppse"

val Scala3Version = "3.9.0"
val betterFilesVersion = "3.9.2"

ThisBuild / organization := "org.openmole"
ThisBuild / version := "1.0-SNAPSHOT"
ThisBuild / resolvers ++= Resolver.sonatypeOssRepos("snapshots")
ThisBuild / resolvers += Resolver.mavenLocal

lazy val ppsePaper = Project("ppse-paper", file("ppse-paper")).settings (
  version := "1.0-SNAPSHOT",
  scalaVersion := Scala3Version,
  resolvers += "jitpack" at "https://jitpack.io",
  libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1",
  libraryDependencies += "com.github.haifengl" % "smile-core" % "4.4.0",
  libraryDependencies += "com.github.openmole" % "JSAT" % "52daf354b728051f22bdb429ca77f561f9791872",
  libraryDependencies += "com.github.pathikrit" %% "better-files" % betterFilesVersion,
  libraryDependencies += "ch.epfl.lamp" %% "gears" % "0.3.1",
  libraryDependencies += "com.github.openmole" % "mgo" % "b16d255912"
)



