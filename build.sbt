enablePlugins(ScalaJSPlugin)
name := "Scala.js Puzzle"
scalaVersion := "2.11.5" // or any other Scala version >= 2.10.2
scalaJSStage in Global := FastOptStage
libraryDependencies += "be.doeraene" %%% "scalajs-jquery" % "0.8.0"
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.8.0"
skip in packageJSDependencies := false
workbenchSettings
bootSnippet := "tutorial.webapp.TutorialApp().main();"
refreshBrowsers <<= refreshBrowsers.triggeredBy(fastOptJS in Compile)
resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"