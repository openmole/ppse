name := "ppse"
organization := "org.openmole"

version := "1.0-SNAPSHOT"

val breezeVersion = "1.2"


lazy val ppse = Project("ppse", file("ppse")).settings (
  scalaVersion := "2.13.6",
  libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1",
  libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.9.1",
  libraryDependencies += "org.openmole" %% "mgo" % "3.51",
  libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.1",
  libraryDependencies += "com.github.haifengl" % "smile-core" % "2.6.0",
  libraryDependencies += "com.edwardraff" % "JSAT" % "0.0.9",
  libraryDependencies ++= Seq(
    "org.scalanlp" %% "breeze" % breezeVersion,
    "org.scalanlp" %% "breeze-natives" % breezeVersion
  ),
  libraryDependencies += "org.locationtech.jts" % "jts-core" % "1.18.1",
  scalacOptions ++= Seq("-Ymacro-annotations", "-language:postfixOps")
)



lazy val selfContained = Project("ppse-paper", file("ppse-paper")).settings (
  scalaVersion := "3.0.1",
  libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1",
  libraryDependencies += "com.edwardraff" % "JSAT" % "0.0.9",
  libraryDependencies ++= Seq(
    "org.scalanlp" %% "breeze" % breezeVersion cross CrossVersion.for3Use2_13,
    "org.scalanlp" %% "breeze-natives" % breezeVersion  cross CrossVersion.for3Use2_13
  )
)

//resolvers += Resolver.sonatypeRepo("snapshots")



