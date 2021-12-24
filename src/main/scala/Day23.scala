import scala.io._

object Day23 extends App:
  val timestamp = System.currentTimeMillis

  case class Pod(category: Char, energyUsage: Int)

  val podA1 = Pod('A', 1)
  val podA2 = Pod('A', 1)
  val podB1 = Pod('B', 10)
  val podB2 = Pod('B', 10)
  val podC1 = Pod('C', 100)
  val podC2 = Pod('C', 100)
  val podD1 = Pod('D', 1000)
  val podD2 = Pod('D', 1000)

  val pods = List(podA1, podA2, podB1, podB2, podC1, podC2, podD1, podD2)

  case class Loc(
      name: String,
      goal: Option[Char] = None,
      capacity: Int,
      stack: List[Pod],
      energyUsed: Int = 0,
    ):

    override def equals(that: Any): Boolean = 
      that match { case loc: Loc if name == loc.name => true ; case _ => false }

    def pop: (Pod,Loc) =
      (stack.head, copy(stack = stack.tail))

    def push(p: Pod, steps: Int): Loc =
      copy(energyUsed = p.energyUsage * steps, stack = p :: stack)

    def isEmpty: Boolean =
      stack.size == 0

    def nonEmpty: Boolean =
      !isEmpty
      
    def hasSpace: Boolean =
      stack.size <= capacity

    def happy: Boolean =
      goal.map(category => stack.forall(_.category == category)).getOrElse(false)

  def go(estate: Estate): List[Estate] =
    val x = for {
      source <- estate.locations if source.nonEmpty
      target <- estate.locations if source != target
      path   <- estate.paths(source).filter(_.last == target) if (path.forall(_.hasSpace))
    } yield (source,target)
    ???


  val answer1: Long = ???
  println(s"Answer 1: $answer1 [${System.currentTimeMillis - timestamp}ms]")
  assert(answer1 == 1160303042684776L)


  // ---

  case class Estate(
    hallLeft: Loc  = Loc("hallLeft",  None, 2, List.empty),
    hallSlot1: Loc = Loc("hallSlot1", None, 1, List.empty),
    hallSlot2: Loc = Loc("hallSlot2", None, 1, List.empty),
    hallSlot3: Loc = Loc("hallSlot3", None, 1, List.empty),
    hallRight: Loc = Loc("hallRight", None, 2, List.empty),

    roomA: Loc = Loc("roomA", Some('A'), 2, List(podB1, podB2)),
    roomB: Loc = Loc("roomB", Some('B'), 2, List(podA1, podC1)),
    roomC: Loc = Loc("roomC", Some('C'), 2, List(podA2, podD1)),
    roomD: Loc = Loc("roomD", Some('D'), 2, List(podD2, podC2))
  ):

    def move(from: Loc, to: Loc): Estate =
      ???

    def happy: Boolean =
      rooms.forall(_.happy)

    def totalEnergyUsed: Int =
      rooms.foldLeft(0)((total,room) => total + room.energyUsed)

    val locations: List[Loc] =
      List(hallLeft, hallRight, hallSlot1, hallSlot2, hallSlot3, hallRight, roomA, roomB, roomC, roomD)

    val rooms: List[Loc] =
      List(roomA, roomB, roomC, roomD)

    val paths: Map[Loc,Set[List[Loc]]] = Map(
      hallLeft -> Set(
        // right
        List(roomA),
        List(hallSlot1),
        List(hallSlot1, roomB),
        List(hallSlot1, hallSlot2),
        List(hallSlot1, hallSlot2, roomC),
        List(hallSlot1, hallSlot2, hallSlot3),
        List(hallSlot1, hallSlot2, hallSlot3, roomD),
        List(hallSlot1, hallSlot2, hallSlot3, hallRight)),
      roomA -> Set(
        // left
        List(hallLeft),
        // right
        List(hallSlot1),
        List(hallSlot1, roomB),
        List(hallSlot1, hallSlot2),
        List(hallSlot1, hallSlot2, roomC),
        List(hallSlot1, hallSlot2, hallSlot3),
        List(hallSlot1, hallSlot2, hallSlot3, roomD),
        List(hallSlot1, hallSlot2, hallSlot3, hallRight)),
      hallSlot1 -> Set(
        // left
        List(roomA),
        List(hallLeft),
        // right
        List(roomB),
        List(hallSlot2),
        List(hallSlot2, roomC),
        List(hallSlot2, hallSlot3),
        List(hallSlot2, hallSlot3, roomD),
        List(hallSlot2, hallSlot3, hallRight)),
      roomB -> Set(
        // left
        List(hallSlot1),
        List(hallSlot1, roomA),
        List(hallSlot1, hallLeft),
        // right
        List(hallSlot2),
        List(hallSlot2, roomC),
        List(hallSlot2, hallSlot3),
        List(hallSlot2, hallSlot3, roomD),
        List(hallSlot2, hallSlot3, hallRight)),
      hallSlot2 -> Set(
        // left
        List(roomB),
        List(hallSlot1),
        List(hallSlot1, roomA),
        List(hallSlot1, hallLeft),
        // right
        List(roomC),
        List(hallSlot3),
        List(hallSlot3, roomD),
        List(hallSlot3, hallRight)),
      roomC -> Set(
        // left
        List(hallSlot2),
        List(hallSlot2, roomB),
        List(hallSlot2, hallSlot1),
        List(hallSlot2, hallSlot1, roomA),
        List(hallSlot2, hallSlot1, hallLeft),
        // right
        List(hallSlot3),
        List(hallSlot3, roomD),
        List(hallSlot3, hallRight)),
      hallSlot3 -> Set(
        // left
        List(roomC),
        List(hallSlot2),
        List(hallSlot2, roomB),
        List(hallSlot2, hallSlot1),
        List(hallSlot2, hallSlot1, roomA),
        List(hallSlot2, hallSlot1, hallLeft),
        // right
        List(roomD),
        List(hallRight)),
      roomD -> Set(
        // left
        List(hallSlot3),
        List(hallSlot3, roomC),
        List(hallSlot3, hallSlot2),
        List(hallSlot3, hallSlot2, roomB),
        List(hallSlot3, hallSlot2, hallSlot1),
        List(hallSlot1, hallSlot2, hallSlot1, roomA),
        List(hallSlot3, hallSlot2, hallSlot1, hallLeft),
        // right
        List(hallRight)),
      hallRight -> Set(
        // left
        List(roomD),
        List(hallSlot3),
        List(hallSlot3, roomC),
        List(hallSlot3, hallSlot2),
        List(hallSlot3, hallSlot2, roomB),
        List(hallSlot3, hallSlot2, hallSlot1),
        List(hallSlot3, hallSlot2, hallSlot1, roomA),
        List(hallSlot3, hallSlot2, hallSlot1, hallLeft),
      )
    )
    