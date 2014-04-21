package psp
package ccmacro

package object api {
  // type TypeParams  = List[Tparam]
  // type ValueParams = List[Vparam]
}


// def onull(s: String): String = if (s eq null) "" else s

// def expand(name: String, tparams: String, vparams: String): String = {
//   printResult("expand")(s"""case class $name$tparams$vparams""")
// }

// def generateCaseClasses() = Def.task {
//   val ccregex = """^([^\[(]+)(\[.*?\])?(\(.*\))$""".r
//   val dir = (sourceManaged in Compile).value
//   def gen(name: String, code: String): File = {
//     val f = dir / s"$name.scala"
//     try f finally IO.write(f, code)
//   }
//   augmentString(caseClasses).lines.toSeq map (s => printResult("line")(s)) flatMap {
//     case ccregex(name, tparams, vparams) => List(gen(name, expand(name, onull(tparams), onull(vparams))))
//     case _                               => Nil
//   }
// }

// def printResult[A](msg: String)(body: A): A = {
//   println(s"$msg: $body")
//   body
// }
