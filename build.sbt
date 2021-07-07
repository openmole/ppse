name := "ppse"

version := "1.0-SNAPSHOT"

scalaVersion := "2.13.6"
val breezeVersion = "1.2"

//resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"
libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.9.1"
libraryDependencies += "org.openmole" %% "mgo" % "3.50-SNAPSHOT"
libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.1"

libraryDependencies ++= Seq(
  "org.scalanlp" %% "breeze" % breezeVersion,
  "org.scalanlp" %% "breeze-natives" % breezeVersion
)

scalacOptions ++= Seq("-Ymacro-annotations", "-language:postfixOps")


