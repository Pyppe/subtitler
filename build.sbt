name := """subtitler"""

version := "1.0"

scalaVersion := "2.11.4"

homepage := Some(url("https://github.com/Pyppe/subtitler"))

libraryDependencies ++= Seq(
  "ch.qos.logback"             %  "logback-classic" % "1.1.2",
  "com.typesafe.play"          %% "play-ws"         % "2.3.7",
  "com.typesafe.scala-logging" %% "scala-logging"   % "3.1.0",
  "commons-io"                 %  "commons-io"      % "2.4",
  "net.ceedubs"                %% "ficus"           % "1.1.2" // Scala-wrapper for Typesafe config
)

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
