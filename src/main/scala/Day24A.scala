import scala.io._
import scala.util.Try

object Day24A extends App:
  val timestamp = System.currentTimeMillis

  type Var = String
  type Val = Long
  case class ALU(variables: Map[Var,Val], stdin: List[Val]):

    def valueOf(varOrVal: Either[Var,Val]): Val =
      varOrVal match
        case Left(v)  => variables(v)
        case Right(i) => i

    def exec(program: List[Inst]): ALU =
      assert(program.size == 252)
      program.foldLeft(this)((alu,inst) => alu.exec(inst))

    def exec(inst: Inst): ALU =
      val alu = inst match
        case Inp(v, _)   => copy(variables = variables.updated(v, stdin.head), stdin = stdin.tail)
        case Add(a,b, _) => copy(variables = variables.updated(a, variables(a) + valueOf(b)))
        case Mul(a,b, _) => copy(variables = variables.updated(a, variables(a) * valueOf(b)))
        case Div(a,b, _) => copy(variables = variables.updated(a, variables(a) / valueOf(b)))
        case Mod(a,b, _) => copy(variables = variables.updated(a, variables(a) % valueOf(b)))
        case Eql(a,b, _) => copy(variables = variables.updated(a, if (variables(a) == valueOf(b)) 1 else 0))
      alu


  sealed trait Inst(comment: String)
  case class Inp(a: Var, c: String)                     extends Inst(c)
  case class Add(a: Var, b: Either[Var,Val], c: String) extends Inst(c)
  case class Mul(a: Var, b: Either[Var,Val], c: String) extends Inst(c)
  case class Div(a: Var, b: Either[Var,Val], c: String) extends Inst(c)
  case class Mod(a: Var, b: Either[Var,Val], c: String) extends Inst(c)
  case class Eql(a: Var, b: Either[Var,Val], c: String) extends Inst(c)

  object Inst:
    import util.matching.*

    val Lit: Regex =
      """(inp|add|mul|div|mod|eql)\s(w|x|y|z)\s?(w|x|y|z|[-+]?\d+)?\s*(.+)?""".r

    def varOrVal(s: String): Either[Var,Val] =
      Try(Right(s.toLong)).getOrElse(Left(s))

  object ALU:
    def init(input: List[Val], w: Val = 0L, x: Val = 0L, y: Val = 0L, z: Val = 0L): ALU =
      ALU(variables = Map("w" -> w, "x" -> x, "y" -> y, "z" -> z), stdin = input)

  val program: List[Inst] =
    import Inst.*

    Source
      .fromFile("src/resources/input24.txt")
      .getLines
      .toList
      .map(_.trim)
      .filter(_.nonEmpty)
      .map {
        case Lit(inst, a, _, c) if inst == "inp" => Inp(a, c)
        case Lit(inst, a, b, c) if inst == "add" => Add(a, varOrVal(b), c)
        case Lit(inst, a, b, c) if inst == "mul" => Mul(a, varOrVal(b), c)
        case Lit(inst, a, b, c) if inst == "div" => Div(a, varOrVal(b), c)
        case Lit(inst, a, b, c) if inst == "mod" => Mod(a, varOrVal(b), c)
        case Lit(inst, a, b, c) if inst == "eql" => Eql(a, varOrVal(b), c)
        case lit => sys.error(s"lit='$lit'")
      }

  var count = 0

  Range(9, 0, -1).map(_.toLong).foreach(d0 =>
  Range(9, 0, -1).map(_.toLong).foreach(d1 =>
  Range(9, 0, -1).map(_.toLong).foreach(d2 =>
  Range(9, 0, -1).map(_.toLong).foreach(d3 =>
    if (d3 == d2 - 8 + 3)
      Range(9, 0, -1).map(_.toLong).foreach(d4 =>
      Range(9, 0, -1).map(_.toLong).foreach(d5 =>
      Range(9, 0, -1).map(_.toLong).foreach(d6 =>
      Range(9, 0, -1).map(_.toLong).foreach(d7 =>
        if (d7 == d6 - 11 + 13)
          // count = count + 1
          // println(s"count=$count [${System.currentTimeMillis - timestamp}ms]")
          Range(9, 0, -1).map(_.toLong).foreach(d8 =>
          Range(9, 0, -1).map(_.toLong).foreach(d9 =>
            if (d9 == d8 - 1 + 10)
              Range(9, 0, -1).map(_.toLong).foreach(d10 =>
                if (d10 == d5 - 8 + 10)
                  Range(9, 0, -1).map(_.toLong).foreach(d11 =>
                    if (d11 == d4 - 5 + 14)
                      Range(9, 0, -1).map(_.toLong).foreach(d12 =>
                        if (d12 == d1 - 16 + 6)
                          Range(9, 0, -1).map(_.toLong).foreach(d13 =>
                            if (d13 == d0 - 6 + 5)
                              val input = List(d0,d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13)
                              if (input.foldLeft("")(_+_) == "39999698799429")
                                if (ALU.init(input).exec(program).variables("z") == 0)
                                  println(s"FOUND = ${input.foldLeft("")(_+_)}")
  ))))))))))))))
