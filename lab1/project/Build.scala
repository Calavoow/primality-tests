import sbt._
import Keys._
import sbtassembly.Plugin.AssemblyKeys._

object BroadcastOrderingBuild extends Build {

	lazy val buildSettings = Defaults.defaultSettings ++ Seq(
		name := "Primality testing",
		version := "0.1",
		scalaVersion := "2.10.3",
    	libraryDependencies ++= Seq(
      	  "org.scalatest" % "scalatest_2.10" % "2.1.0" % "test"
    	)
	)

  lazy val node = Project(
    "node",
    file("."),
    settings = buildSettings ++ sbtassembly.Plugin.assemblySettings ++ Seq(
      jarName in assembly := "primality.jar"
    )
  )
}
