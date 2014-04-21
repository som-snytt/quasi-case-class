package psp
package ccmacro

import scala.util.matching.Regex
import scala.reflect.NameTransformer.decode

trait Text {
  private def replacements = List(
    "List()"                                 -> "Nil",
    "TypeTree()"                             -> "_",
    "Modifiers()"                            -> "_",
    "noSelfType"                             -> "_",
    "Ident(termNames.WILDCARD)"              -> "_",
    """TypeName[(](.*?)[)]""".r              -> "$1",
    """TermName[(](.*?)[)]""".r              -> "$1",
    """Literal[(]Constant[(](.*?)[)][)]""".r -> "Lit($1)"
  )

  def pp(str: String): String = {
    val str1 = replacements.foldLeft(str) {
      case (res, (in: String, out)) => res.replaceAllLiterally(in, out)
      case (res, (in: Regex, out))  => res.replaceAll(in.toString, out)
    }
    val chars   = str1.toCharArray
    var index   = 0
    val lparens = (0 until chars.length).toVector filter (idx => chars(idx) == '(')
    val fixed: Vector[(Int, Int)]   = lparens flatMap { lpindex =>
      def loop(rindex: Int): Option[(Int, Int)] = chars(rindex) match {
        case '(' => None
        case ')' => Some(lpindex -> rindex)
        case _   => loop(rindex + 1)
      }
      loop(lpindex + 1)
    }
    val fixedMap: Map[Int, String] = fixed map { case (l, r) => l -> str1.substring(l, r + 1) } toMap
    val sb = new StringBuilder

    def loop(index: Int, depth: Int): Unit = {
      def ch     = chars(index)
      def nextch = if (index + 1 >= chars.length) (0: Char) else chars(index + 1)

      def indent(depth: Int): Unit = {
        sb append ch append '\n' append ("  " * depth)
        loop(index + 1, depth)
      }
      if (index < chars.length) fixedMap get index match {
        case Some(str)                       => sb append str ; loop(index + str.length, depth)
        case _ if ch == '('                  => indent(depth + 1)
        case _ if ch == ')' && nextch == ',' => sb append "),\n" append "  " * (depth - 1) ; loop(index + 2, depth - 1)
        case _ if ch == ')'                  => indent(depth - 1)
        case _                               => sb append ch ; loop(index + 1, depth)
      }
    }

    loop(0, 0)
    sb.result
  }
}

object Text extends Text
