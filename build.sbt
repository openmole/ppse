name := "ppse"

val breezeVersion = "2.0.1-RC2"
val circeVersion = "0.14.5"

ThisBuild / organization := "org.openmole"
ThisBuild / version := "1.0-SNAPSHOT"

lazy val ppse = Project("ppse", file("ppse")).settings (
  scalaVersion := "3.2.2",
  libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1",
  libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.9.1" cross CrossVersion.for3Use2_13,
  libraryDependencies += "org.openmole" %% "mgo" % "3.55",
  libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.1",
  libraryDependencies += "com.github.haifengl" % "smile-core" % "2.6.0",
  libraryDependencies += "com.edwardraff" % "JSAT" % "0.0.9",
  libraryDependencies ++= Seq(
    "org.scalanlp" %% "breeze" % breezeVersion,
    "org.scalanlp" %% "breeze-natives" % breezeVersion
  ),
  libraryDependencies += "org.locationtech.jts" % "jts-core" % "1.18.1",
  libraryDependencies += "org.plotly-scala" %% "plotly-render" % "0.8.4" cross CrossVersion.for3Use2_13,
    scalacOptions ++= Seq("-Ymacro-annotations", "-language:postfixOps"),

  libraryDependencies ++= Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic",
    "io.circe" %% "circe-parser"
  ).map(_ % circeVersion),

  excludeDependencies += ExclusionRule(organization = "org.typelevel", name = "cats-kernel_2.13")
)

lazy val visu = Project("visu", file("visu")).enablePlugins(ScalaJSPlugin).settings (
  scalaVersion := "3.2.2",
  libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.2.0",
  scalaJSUseMainModuleInitializer := true
//  libraryDependencies += "org.plotly-scala" %% "plotly-render" % "0.8.4",
//  libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.9.1"
)

lazy val selfContained = Project("ppse-paper", file("ppse-paper")).settings (
  version := "1.0-SNAPSHOT",
  scalaVersion := "3.2.2",
  libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1",
  libraryDependencies += "com.edwardraff" % "JSAT" % "0.0.9",
  libraryDependencies ++= Seq(
    "org.scalanlp" %% "breeze" % breezeVersion cross CrossVersion.for3Use2_13,
    "org.scalanlp" %% "breeze-natives" % breezeVersion  cross CrossVersion.for3Use2_13
  )
)

//resolvers += Resolver.sonatypeRepo("snapshots")



