organization := "com.github.kmizu"

name := "toys"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.7"

publishMavenStyle := true

val scaladocBranch = settingKey[String]("branch name for scaladoc -doc-source-url")

scaladocBranch := "master"

scalacOptions in (Compile, doc) ++= { Seq(
  "-sourcepath", baseDirectory.value.getAbsolutePath,
  "-doc-source-url", s"https://github.com/kmizu/toys/tree/${scaladocBranch.value}€{FILE_PATH}.scala"
)}

testOptions in Test += Tests.Argument("-u", "target/scalatest-reports")

scalacOptions <++= scalaVersion map { v =>
  Seq("-unchecked", "-deprecation", "-feature", "-language:implicitConversions")
}


libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
  "junit" % "junit" % "4.7" % "test",
  "org.scalatest" %% "scalatest" % "2.1.6" % "test"
)


initialCommands in console += {
  Iterator().map("import "+).mkString("\n")
}

pomExtra := (
  <url>https://github.com/kmizu/toys</url>
  <licenses>
    <license>
      <name>The MIT License</name>
      <url>http://www.opensource.org/licenses/MIT</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:kmizu/toys.git</url>
    <connection>scm:git:git@github.com:kmizu/toys.git</connection>
  </scm>
  <developers>
    <developer>
      <id>kmizu</id>
      <name>Kota Mizushima</name>
      <url>https://github.com/kmizu</url>
    </developer>
  </developers>
) 

publishTo <<= version { v =>
  val nexus = "https://oss.sonatype.org/"
  if (v.endsWith("-SNAPSHOT"))
    Some("snapshots" at nexus+"content/repositories/snapshots")
  else
    Some("releases" at nexus+"service/local/staging/deploy/maven2")
}

credentials ++= {
  val sonatype = ("Sonatype Nexus Repository Manager", "oss.sonatype.org")
  def loadMavenCredentials(file: java.io.File) : Seq[Credentials] = {
    xml.XML.loadFile(file) \ "servers" \ "server" map (s => {
      val host = (s \ "id").text
      val realm = if (host == sonatype._2) sonatype._1 else "Unknown"
      Credentials(realm, host, (s \ "username").text, (s \ "password").text)
    })
  }
  val ivyCredentials   = Path.userHome / ".ivy2" / ".credentials"
  val mavenCredentials = Path.userHome / ".m2"   / "settings.xml"
  (ivyCredentials.asFile, mavenCredentials.asFile) match {
    case (ivy, _) if ivy.canRead => Credentials(ivy) :: Nil
    case (_, mvn) if mvn.canRead => loadMavenCredentials(mvn)
    case _ => Nil
  }
}
