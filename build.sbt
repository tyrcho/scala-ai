
scalaVersion := "2.12.11" //, "2.13.2")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.5" % Test,
  "org.powermock" % "powermock-api-mockito" % "1.5.5" % Test,
  "junit" % "junit" % "4.10" % Test,
  "ch.qos.logback" % "logback-classic" % "1.1.2" ,
  "jfree" % "jfreechart" % "1.0.13" ,
  "org.clapper" %%"grizzled-slf4j" % "1.3.0"
)

Global / onChangedBuildSource := ReloadOnSourceChanges
