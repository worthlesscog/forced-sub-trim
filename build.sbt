name := "Forced Subtitle Trim"

scalaVersion := "2.12.8"

scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-language:implicitConversions",
    "-language:postfixOps",
    "-language:reflectiveCalls",
    "-unchecked"
)

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

libraryDependencies += "io.spray" %% "spray-json" % "1.3.3"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

mainClass in assembly := Some("com.worthlesscog.video.ForcedSubTrim")
