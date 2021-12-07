import scala.io._
import scala.collection.immutable.HashSet

object Day06 extends App:
  val start = System.currentTimeMillis

  val init: Map[Int,Long] =
    Source
      .fromFile("src/resources/input06.txt")
      .getLines
      .toList
      .flatMap(_.split(",").map(_.trim.toInt))
      .groupBy(identity)
      .map((age,fishes) => (age,fishes.size.toLong))

  val TimeToSpawn     = 6
  val InitTimeToSpawn = 8
  val SpawnNow        = -1
  
  def cycle(generation: Map[Int,Long], n: Int = 0): Map[Int,Long] =
    val aged =
      generation
        .map((timeToSpawn,count) => (timeToSpawn - 1) -> count)
    val spawnCount =
      aged.getOrElse(SpawnNow, 0L)
    val timeToSpawnCount =
      aged.getOrElse(TimeToSpawn, 0L) + spawnCount

    aged.removed(SpawnNow) +
      (InitTimeToSpawn -> spawnCount) +
      (TimeToSpawn -> timeToSpawnCount)

  val answer =
    println(s"Init generation count ${init.values.sum}")
    (1 to 256).foldLeft(init)((gen,year) =>
      val next = cycle(gen)
      println(s"\u001b[FYear $year, generation count ${next.values.sum}")
      next
    ).values.sum

  println(s"Answer = ${answer} [${System.currentTimeMillis - start}ms]")
  assert(answer == 1710166656900L)
