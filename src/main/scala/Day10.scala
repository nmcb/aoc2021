import scala.io._
import scala.quoted.Expr

object Day10 extends App:
  val start = System.currentTimeMillis

  val lines =
    Source
      .fromFile("src/main/resources/input10.txt")
      .getLines
      .toList


  def loop(s: String, cur: List[Char] = List.empty[Char]): Option[Char] =
    if (s.isEmpty && cur.isEmpty) None else {
      s.toList match {
        case Nil => None // incomplete
        case '(' :: cs                                => loop(cs.mkString(""), '(' :: cur)
        case '[' :: cs                                => loop(cs.mkString(""), '[' :: cur)
        case '{' :: cs                                => loop(cs.mkString(""), '{' :: cur)
        case '<' :: cs                                => loop(cs.mkString(""), '<' :: cur)
        case ')' :: cs if cur.headOption == Some('(') => loop(cs.mkString(""), cur.tail)
        case ']' :: cs if cur.headOption == Some('[') => loop(cs.mkString(""), cur.tail)
        case '}' :: cs if cur.headOption == Some('{') => loop(cs.mkString(""), cur.tail)
        case '>' :: cs if cur.headOption == Some('<') => loop(cs.mkString(""), cur.tail)
        case c :: _ => Some(c)
      }
    } 


  val answer1 =
    lines.map(loop(_)).filterNot(_.isEmpty).map {
        case Some(')') => 3L
        case Some(']') => 57L
        case Some('}') => 1197L
        case Some('>') => 25137L
        case _ => sys.error(s"boom!")
      }
      .groupMapReduce(identity)(identity)(_+_)
      .values.sum

  println(s"Answer 1 = ${answer1} [${System.currentTimeMillis - start}ms]")
  assert(answer1 == 268845)

  def loop2(s: String, cur: List[Char] = List.empty[Char]): List[Char] =
    if (s.isEmpty && cur.isEmpty) Nil else {
      s.toList match {
        case Nil => cur // incomplete
        case '(' :: cs                                => loop2(cs.mkString(""), '(' :: cur)
        case '[' :: cs                                => loop2(cs.mkString(""), '[' :: cur)
        case '{' :: cs                                => loop2(cs.mkString(""), '{' :: cur)
        case '<' :: cs                                => loop2(cs.mkString(""), '<' :: cur)
        case ')' :: cs if cur.headOption == Some('(') => loop2(cs.mkString(""), cur.tail)
        case ']' :: cs if cur.headOption == Some('[') => loop2(cs.mkString(""), cur.tail)
        case '}' :: cs if cur.headOption == Some('{') => loop2(cs.mkString(""), cur.tail)
        case '>' :: cs if cur.headOption == Some('<') => loop2(cs.mkString(""), cur.tail)
        case c :: _ => Nil
      }
    } 

  val answer2 =
    val scores = lines.map(loop2(_)).filterNot(_.isEmpty).map { stack =>
      stack.foldLeft(0L)((res,c) => c match {
        case '(' => (res * 5) + 1
        case '[' => (res * 5) + 2
        case '{' => (res * 5) + 3
        case '<' => (res * 5) + 4
      })
    }.sorted
    scores(scores.length / 2)

  println(s"Answer 2 = ${answer2} [${System.currentTimeMillis - start}ms]")
  assert(answer2 == 4038824534L)
