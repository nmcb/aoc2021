import scala.io._

object Day15 extends App:

  val lines: Seq[String] =
    Source
      .fromFile("src/resources/input15.txt")
      .getLines
      .toSeq

  type Vertex = Long
  object Vertex:
    def apply(x: Int, y: Int): Vertex =
      (x.toLong << 32) + y

    inline def x(v: Vertex)  =
      (v >>> 32).toInt

    inline def y(v: Vertex): Int =
      (v & 0x0000_FFFF).toInt

  case class Edge(from: Vertex, to: Vertex, weight: Int)

  case class Calc(edgeTo: Map[Vertex, Edge], distTo: Map[Vertex, Int]):
    def pathTo(Vertex: Vertex): Seq[Edge] =
      @scala.annotation.tailrec
      def go(edges: Seq[Edge], cur: Vertex): Seq[Edge] =
        edgeTo.get(cur) match {
          case Some(e) => go(e +: edges, e.from)
          case None => edges
        }
      if (!hasPath(Vertex)) Seq.empty else go(Seq.empty, Vertex)

    def hasPath(Vertex: Vertex): Boolean =
      distTo.contains(Vertex)

    def distToV(Vertex: Vertex): Int =
      distTo(Vertex)

  case class Graph(adjecent: Map[Vertex, Seq[Edge]] = Map.empty):

    def add(e: Edge): Graph =
      Graph(adjecent.updatedWith(e.from)(_.map(_ :+ e).orElse(Some(List(e)))))

    def run(from: Vertex): Calc =
      import scala.collection.mutable

      val edgeTo = mutable.Map.empty[Vertex, Edge]
      val distTo = mutable.Map.from(adjecent.map((Vertex,_) => Vertex -> Int.MaxValue))

      distTo(from) = 0
      val sourceDist = (from, distTo(from))
      val sortByWeight: Ordering[(Vertex, Int)] = (a, b) => a._2.compareTo(b._2)
      val queue = mutable.PriorityQueue[(Vertex, Int)](sourceDist)(sortByWeight)

      while (queue.nonEmpty) {
        val (minDestV, _) = queue.dequeue()
        val edges = adjecent.getOrElse(minDestV, List.empty)

        edges.foreach { e =>
          val riskToTo   = distTo(e.to)
          val riskToFrom = distTo(e.from)
          if (riskToTo > riskToFrom + e.weight)
            distTo(e.to) = riskToFrom + e.weight
            edgeTo(e.to) = e
            if (!queue.exists(_._1 == e.to)) queue.enqueue((e.to, riskToTo))
        }
      }
      Calc(edgeTo.toMap, distTo.toMap)



  def neighbours(x: Int, y: Int)(mx: Int, my: Int): Seq[(Int, Int)] =
    Seq((-1, 0),(1, 0), (0, -1),(0, 1))
      .map((dx, dy) => (x + dx, y + dy))
      .filterNot((x, y) => x < 0 || x > mx || y < 0 || y > my)

  val maxX: Int = lines.head.size - 1
  val maxY: Int = lines.size - 1
  val graph1 =
    lines
      .map(_.toSeq.zipWithIndex)
      .zipWithIndex
      .flatMap((xs,y) => xs.map((w,x) => (x,y,w.toString.toInt)))
      .foldLeft(Graph())((graph,node) =>
        val (x, y, w) = node
        neighbours(x, y)(maxX, maxY)
          .foldLeft(graph)((graph,neighbour) =>
            val (nx, ny) = neighbour
            graph.add(Edge(Vertex(nx, ny), Vertex(x, y), w)))
      )

  val start = System.currentTimeMillis
  println(s"Answer 1 = ${graph1.run(Vertex(0,0)).distToV(Vertex(maxX, maxY))} [${System.currentTimeMillis - start}ms]")

  val extended: Seq[(Int, Int, Int)] =
    lines
      // rows
      .map(l =>
        (1 to 5).foldLeft("")((nl,_) => nl + l))
      .transpose
      .map(_.mkString(""))
      // columns
      .map(c =>
        (1 to 5).foldLeft("")((nc,_) => nc + c))
      .transpose
      .map(_.mkString(""))
      // weights
      .map(_.toSeq.zipWithIndex)
      .zipWithIndex
      .flatMap((xs,y) =>
        xs.map((w,x) =>
          val rights    = x / (maxX + 1)
          val downs     = y / (maxY + 1)
          val additions = rights + downs
          val weight    = w.toString.toInt
          val risk      = ((weight - 1 + additions) % 9) + 1
          (x, y, risk)
      ))

  val extMaxX = ((maxX + 1) * 5) - 1
  val extMaxY = ((maxY + 1) * 5) - 1
  val graph2 =
    extended
      .foldLeft(Graph())((graph,node) =>
        val (x, y, w) = node
        neighbours(x, y)(extMaxX, extMaxY)
          .foldLeft(graph)((graph,neighbour) =>
            val (nx, ny) = neighbour
            graph.add(Edge(Vertex(nx, ny), Vertex(x, y), w)))
      )

  println(s"Answer 2 = ${graph2.run(Vertex(0,0)).distToV(Vertex(extMaxX, extMaxY))} [${System.currentTimeMillis - start}ms]")
