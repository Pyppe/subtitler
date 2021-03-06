name := """subtitler"""

version := "1.0"

scalaVersion := "2.11.4"

homepage := Some(url("https://github.com/Pyppe/subtitler"))

libraryDependencies ++= Seq(
  // Logging
  "ch.qos.logback"             %  "logback-classic" % "1.1.2",
  "org.slf4j"                  %  "slf4j-api"       % "1.7.9",
  "com.typesafe.scala-logging" %% "scala-logging"   % "3.1.0",

  // Http
  "net.databinder.dispatch"    %% "dispatch-core"   % "0.11.2",

  // Json
  "com.typesafe.play"          %% "play-json"       % "2.3.7",

  // Misc
  "commons-io"                 % "commons-io"       % "2.4",
  "org.apache.commons"         % "commons-lang3"    % "3.3.2",
  "org.joda"                   % "joda-convert"     % "1.7",
  "joda-time"                  % "joda-time"        % "2.6",
  "org.fusesource.jansi"       % "jansi"            % "1.11",  // Colors
  "net.ceedubs"                %% "ficus"           % "1.1.2", // Scala-wrapper for Typesafe config
  "com.github.scopt"           %% "scopt"           % "3.3.0", // Command-line args parser

  // Test
  "org.specs2"                 %% "specs2-core"     % "2.4.15" % "test"
)

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

assemblyJarName in assembly := "subtitler.jar"

assemblyMergeStrategy in assembly := {
  case PathList("com", "fasterxml", "jackson", "databind", xs @ _*) => MergeStrategy.first
  case PathList("play", xs @ _*)                                    => MergeStrategy.first
  case PathList("org", "apache", "xerces", xs @ _*)                 => MergeStrategy.first
  case PathList("scala", "concurrent", xs @ _*)                     => MergeStrategy.first
  case PathList("scala", "reflect", "internal", xs @ _*)            => MergeStrategy.first
  case "logback.xml"                                                => MergeStrategy.first
  case "subtitler.conf"                                             => MergeStrategy.discard
  case x =>
    (assemblyMergeStrategy in assembly).value(x) // Use the old strategy as default
}
