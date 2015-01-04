name := """subtitler"""

version := "1.0"

scalaVersion := "2.11.4"

homepage := Some(url("https://github.com/Pyppe/subtitler"))

libraryDependencies ++= Seq(
  //Logging
  "ch.qos.logback"             %  "logback-classic" % "1.1.2",
  "org.slf4j"                  %  "slf4j-api"       % "1.7.9",
  "com.typesafe.scala-logging" %% "scala-logging"   % "3.1.0",

  // Http
  "com.typesafe.play"          %% "play-ws"         % "2.3.7" excludeAll(
    ExclusionRule(organization = "commons-logging")
  ),

  // Misc
  "commons-io"                 %  "commons-io"      % "2.4",
  "net.ceedubs"                %% "ficus"           % "1.1.2", // Scala-wrapper for Typesafe config
  "org.rogach"                 %% "scallop"         % "0.9.5"

)

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

assemblyMergeStrategy in assembly := {
  case PathList("com", "fasterxml", "jackson", "databind", xs @ _*) => MergeStrategy.first
  case PathList("play", xs @ _*)                                    => MergeStrategy.first
  case PathList("org", "apache", "xerces", xs @ _*)                 => MergeStrategy.first
  case PathList("scala", "concurrent", xs @ _*)                     => MergeStrategy.first
  case PathList("scala", "reflect", "internal", xs @ _*)            => MergeStrategy.first
  case "logback.xml"                                                => MergeStrategy.first
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}
