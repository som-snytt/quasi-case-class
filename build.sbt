val imports = """
import psp._, ccmacro._, api._
import Materializer._, u._, internal._
import java.nio.file.Path
""".trim

def depends = Def setting Seq(
  "org.scala-lang" % "scala-compiler"  % scalaVersion.value,
  "com.novocode"   % "junit-interface" %       "0.10"        % "test"
)

lazy val root = project in file(".") settings (
                       name  :=  "ccmacro",
               organization  :=  "org.improving",
                    version  :=  "1.0.0-SNAPSHOT",
               scalaVersion  :=  "2.11.1",
 initialCommands in console  :=  imports,
                fork in run  :=  true,
            fork in console  :=  true,
               fork in Test  :=  true,
  parallelExecution in Test  :=  false,
                   licenses  :=  Seq("Apache" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
                shellPrompt  :=  (s => """%s#%s>""".format(name.value, (Project extract s).currentRef.project)),
                logBuffered  :=  false,
   scalacOptions in Compile ++=  Seq("-language:_"),
        libraryDependencies <++= depends
)

//   scalacOptions in Compile ++= Seq("-language:*", "-Yquasiquote-debug"),
// sourceGenerators in Compile <+= generateCaseClasses()
