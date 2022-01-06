ThisBuild / scalaVersion     := "2.13.7"
ThisBuild / version := "0.1.0.0"

lazy val root = (project in file("."))
  .settings(
    Compile / unmanagedSourceDirectories := Nil,
    Test / unmanagedSourceDirectories := Nil
  )
  .enablePlugins(ParadoxPlugin)
  .enablePlugins(ParadoxSitePlugin)
  .enablePlugins(GitHubPagesPlugin)
  .settings(
    scalacOptions := Nil,
    paradoxTheme := Some(builtinParadoxTheme("generic")),
    gitHubPagesOrgName := "qwbarch",
    gitHubPagesRepoName := "captcha-haskell",
    gitHubPagesSiteDir := baseDirectory.value / "target/site"
  )
