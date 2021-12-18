import scala.io.*

object Day18 extends App:
  val start = System.currentTimeMillis

  enum Num derives CanEqual:
    case Pair(left: Num, right: Num)
    case Lit(value: Long)

    override def toString: String = this match
      case Pair(left, right) => s"[$left,$right]"
      case Lit(value)        => value.toString

    case class Val(value: Long, depth: Int, index: Int)

    def values: Iterator[Val] =
      def go(n: Num, d: Int): Iterator[(Long, Int)] =
        n match
          case Pair(l, r) => go(l, d + 1) ++ go(r, d + 1)
          case Lit(value) => Iterator((value, d - 1))
      go(this, 0).zipWithIndex.map {
        case ((v, d), i) => Val(v, d, i)
      }

    def +(that: Num): Num =
      Pair(this, that).reduce

    def addl(value: Long, index: Int): Num =
      values.takeWhile(_.index < index).toSeq.lastOption match
        case Some(Val(_, _, i)) => update(value, i)
        case None               => this

    def addr(value: Long, index: Int): Num =
      values.dropWhile(_.index <= index).toSeq.headOption match
        case Some(Val(_, _, i)) => update(value, i)
        case None               => this

    def update(add: Long, index: Int): Num =
      def go(ix: Int, num: Num): (Int, Num) =
        num match
          case Pair(l, r) =>
            val (li, ln) = go(ix, l)
            val (ri, rn) = go(li, r)
            (ri, Pair(ln, rn))
          case Lit(v) =>
            (ix + 1, Lit(if ix == index then v + add else v))
      go(0, this)._2

    def magnitude: Long =
      this match
        case Lit(v)     => v
        case Pair(l, r) => 3 * l.magnitude + 2 * r.magnitude

    def reduce: Num =
      explode.orElse(split) match
        case None      => this
        case Some(num) => num.reduce

    def explode: Option[Num] =
      values.filter(_.depth == 4).take(2).toSeq match
        case Seq(Val(l, _, li), Val(r, _, ri)) =>
          Some(addl(l, li).addr(r, ri).fix(li))
        case _ =>
          None

    def fix(index: Int): Num =
      def go(ix: Int, depth: Int, num: Num): (Int, Num) =
        num match
          case Pair(l, r) =>
            val (li, ln) = go(ix, depth + 1, l)
            val (ri, rn) = go(li, depth + 1, r)
            (ri, if ix == index && depth == 4 then Lit(0) else Pair(ln, rn))
          case Lit(v) => (ix + 1, Lit(v))
      go(0, 0, this)._2

    def split: Option[Num] =
      this match
        case Lit(v) if v >= 10 =>
          Some(Pair(Lit(v / 2), Lit(v / 2 + v % 2)))
        case Lit(_)            =>
          None
        case Pair(l, r)        =>
          (l.split, r.split) match
            case (None, None)    => None
            case (Some(s), _)    => Some(Pair(s, r))
            case (None, Some(s)) => Some(Pair(l, s))

  import Num.*

  def parse(s: String): Num =
    import parsing.*
    import P.*    
    def vp: P[Num] = digits.map(Lit.apply)
    def pp: P[Num] = for { _ <- char('[') ; l <- np ; _ <- char(',') ; r <- np ; _ <- char(']') } yield Pair(l, r)
    def np: P[Num] = vp | pp
    run(np)(s)  

  val input: List[String] =
    Source
      .fromFile("src/resources/input18.txt")
      .getLines
      .toList

  val answer1: Long =
    input
      .map(parse)
      .reduceLeft(_ + _)
      .magnitude

  println(s"answer 1: $answer1 [${System.currentTimeMillis - start}ms]")
  assert(answer1 == 3935)
  
  val answer2: Long =
    input
      .map(parse)
      .combinations(2)
      .flatMap {
        case List(x, y) => List(x + y, y + x)
        case _ => sys.error("boom")
      }
      .map(_.magnitude)
      .max

  println(s"answer 2: $answer2 [${System.currentTimeMillis - start}ms]")
  assert(answer2 == 4669)
