import scala.io._

object Day22 extends App:
  val timestamp = System.currentTimeMillis

  case class Step1(on: Boolean, xr: Range, yr: Range, zr: Range)

  val StepLit =
    """(on|off) x=([-+]?\d+)..([-+]?\d+),y=([-+]?\d+)..([-+]?\d+),z=([-+]?\d+)..([-+]?\d+)""".r

  val steps1: Seq[Step1] =
    Source
      .fromFile("src/resources/input22.txt")
      .getLines
      .toSeq
      .map(_.trim)
      .map {
        case StepLit(set, x0, x1, y0, y1, z0, z1) =>
          import math._
          val xr = min(x0.toInt, x1.toInt) to max(x0.toInt, x1.toInt)
          val yr = min(y0.toInt, y1.toInt) to max(y0.toInt, y1.toInt)
          val zr = min(z0.toInt, z1.toInt) to max(z0.toInt, z1.toInt)
          val on = set == "on" 
          Step1(on, xr, yr, zr)
      }
    
  case class Pos(x: Int, y: Int, z: Int):

    def +(that: Pos): Pos =
      Pos(x + that.x, y + that.y, z + that.z)

    def <=(that: Pos): Boolean =
      x <= that.x && y <= that.y && z <= that.z

    def min(that: Pos): Pos =
      Pos(x.min(that.x), y.min(that.y), z.min(that.z))

    def max(that: Pos): Pos =
      Pos(x.max(that.x), y.max(that.y), z.max(that.z))


  case class Cube1(all: Map[Pos,Boolean]):

    def getOrElse(p: Pos, b: Boolean): Boolean =
      all.getOrElse(p, b)

    def update(pos: Pos, bool: Boolean): Cube1 =
      map((p,b) => if (pos == p) bool else b)

    def update(g: Cube1): Cube1 =
      map((p,b) => g.getOrElse(p, b))

    def map(f: (Pos,Boolean) => Boolean): Cube1 =
      Cube1(all.map((p,a) => p -> f(p,a)))

  object Cube1:

    def cuboid(xr: Range, yr: Range, zr: Range)(b: Boolean): Cube1 =
      val all = for (x <- xr ; y <- yr ; z <- zr) yield Pos(x, y, z)
      Cube1(all.groupMapReduce(identity)(_ => b)(_ && _))

    def cuboid(s: Step1): Cube1 =
      cuboid(s.xr, s.yr, s.zr)(s.on)

    def inside(r: Range): Boolean =
      r.start >= -50 && r.end <= 50

    def inside(s: Step1): Boolean =
      inside(s.xr) && inside(s.yr) && inside(s.zr)

    val init = cuboid(-50 to 50, -50 to 50, -50 to 50)(false)
    def reboot(steps1: Seq[Step1], grid: Cube1 = init): Cube1 =
      steps1
        .filter(inside)
        .foldLeft(grid)((g,s) => g.update(cuboid(s)))

  val answer1 = Cube1.reboot(steps1).all.values.count(identity)
  println(s"answer 1: $answer1 [${System.currentTimeMillis - timestamp}ms]")
  assert(answer1 == 542711)

  case class Cube(min: Pos, max: Pos): 

    def size: Long =
      val xs: Long = max.x - min.x + 1
      val ys: Long = max.y - min.y + 1
      val zs: Long = max.z - min.z + 1
      xs * ys * zs

    def union(that: Cube): Cube =
      val nmin = min.min(that.min)
      val nmax = max.max(that.max)
      Cube(nmin, nmax)

    def intersect(that: Cube): Option[Cube] =
      val nmin = min.max(that.min)
      val nmax = max.min(that.max)
      if (nmin <= nmax) Some(Cube(nmin, nmax)) else None

  case class Step(on: Boolean, cube: Cube)
  
  object Cube:

    def reboot(steps: Seq[Step]): Long =
      // recursive descent on all steps for each step
      def go(splits: Seq[(Step,Set[Int])], signum: Int, found: Long): Long =
        val count = splits.map((s,_) => if (s.on) s.cube.size else 0L).sum
        val acc   = found + signum * count

        if (splits.nonEmpty) 
          val todo: Seq[(Step, Set[Int])] =
            for {
              (split, indices) <- splits
              (step, index) <- steps.zipWithIndex
              if index > indices.max && (split.on || !step.on)
              intesection <- split.cube.intersect(step.cube)
            } yield (Step(step.on || split.on, intesection), indices + index)
          go(todo, -signum, acc)
        else
          acc

      val init = steps.zipWithIndex.map((step,index) => (step, Set(index)))
      go(init, +1, 0L)

  lazy val steps2 =
    Source
      .fromFile("src/resources/input22.txt")
      .getLines
      .map {
        case StepLit(on, x0, x1, y0, y1, z0, z1) =>
          val min = Pos(x0.toInt, y0.toInt, z0.toInt)
          val max = Pos(x1.toInt, y1.toInt, z1.toInt)
          val set = on == "on"
          Step(set, Cube(min, max))
      }
      .toSeq

  val answer2: Long = Cube.reboot(steps2)
  println(s"answer 2: $answer2 [${System.currentTimeMillis - timestamp}ms]")
  assert(answer2 == 1160303042684776L)

    