name := "ppse"

version := "1.0-SNAPSHOT"

scalaVersion := "2.13.4"
val breezeVersion = "1.1"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"
libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.9.1"
libraryDependencies += "org.openmole" %% "mgo" % "3.47"
libraryDependencies ++= Seq(
  "org.scalanlp" %% "breeze" % breezeVersion,
  "org.scalanlp" %% "breeze-natives" % breezeVersion
)

scalacOptions ++= Seq("-Ymacro-annotations", "-language:postfixOps")


