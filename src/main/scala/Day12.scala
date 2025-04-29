import scala.io._


object Day12 extends App:
  val start = System.currentTimeMillis

  def ﷽[A](a: A): A = a

  val graph: List[(Node,Node)] =
    Source
      .fromFile("src/main/resources/input12.txt")
      .getLines
      .map(l => l.split("-").toList).toList
      .flatMap(st => List((Node(st(0)),Node(st(1))), (Node(st(1)),Node(st(0)))))
      .filterNot((source,target) => source.isEnd || target.isStart)

  case class Node(value: String) {
    import Node._
    def isStart = this == Start
    def isEnd   = this == End
    def isSmall = value.map(_.toLower) == value
    override def toString: String = value
  }
  object Node {
    val Start = Node("start")
    val End   = Node("end")
  }
  import Node._

  def find1(
    route: List[Node]        = List(Start),
    steps: List[(Node,Node)] = graph,
    acc:   List[List[Node]]  = List.empty
  ): List[List[Node]] =
    def targets(source: Node): List[Node] =
      steps.filter(_._1 == source).map(_._2)
    def prunedSteps(reached: Node, via: Option[Node]): List[(Node,Node)] =
      if (reached.isSmall)
        steps.filterNot((s,t) => t == reached)
      else
        via.map(from => steps.filterNot((s,t) => s == from && t == reached)).getOrElse(steps)

    route match {
      case End :: rest =>
        find1(List(Start), prunedSteps(End, rest.headOption), (End :: rest).reverse :: acc)
      case node :: rest =>
        val loop = targets(node).flatMap(target => find1(target :: node :: rest, prunedSteps(node, rest.headOption), acc))
        acc ++ loop
      case _ =>
        sys.error("boom!")
    }

  val pathsPart1 = find1().distinct
  println(s"Answer 1 = ${pathsPart1.size} [${System.currentTimeMillis - start}ms]")

  def find2(
    route: List[Node]        = List(Start),
    visited: Map[Node,Int]   = graph.filter((_,t) => t.isSmall).map((_,t) => t -> 0).distinct.toMap,
    acc:   List[List[Node]]  = List.empty
  ): List[List[Node]] =

    def targets(source: Node, counts: Map[Node,Int]): List[Node] =
      assert(counts.map((t,c) => c).count(_ >= 2) <= 1)
      val has2 = counts.map((t,c) => c).max == 2
      val maxv = if (has2) 1 else 2
      val scts = graph.filter((s,t) => s == source && t.isSmall && counts(t) < maxv)
      val bcts = graph.filter((s,t) => s == source && !t.isSmall)
      (scts ++ bcts).map((s,t) => t)

    def visit(reached: Node, counts: Map[Node,Int]): Map[Node,Int] =
      if (reached.isSmall && !reached.isStart)
        counts.updated(reached, counts(reached) + 1)
      else
        counts

    route match {
      case End :: rest if !(acc.contains((End :: rest).reverse))=>
        val path = (End :: rest).reverse
        find2(List(Start), visited, path :: acc)
      case node :: rest =>
        val counts = visit(node, visited)
        val loop = targets(node, counts).flatMap(target =>
          find2(target :: node :: rest, counts, acc)
        )
        acc ++ loop
      case _ =>
        sys.error("boom!")
    }

  val pathsPart2 = find2().distinct
  println(s"Answer 2 = ${pathsPart2.size} [${System.currentTimeMillis - start}ms]")
