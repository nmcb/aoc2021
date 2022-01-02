import scala.collection.*

object Day21 extends App:
  val timestamp = System.currentTimeMillis

  trait Dice:
    def face: Int
    def roll: Seq[Dice]
    def rolled: Int

  case class CyclingDice(min: Int, max: Int, cur: Int = 0, rolled: Int = 0) extends Dice:
    def face: Int =
      cur
    def roll: Seq[Dice] =
      if (cur >= max) Seq(copy(cur = min, rolled = rolled + 1))
      else Seq(copy(cur = cur + 1, rolled = rolled + 1))

  case class Player(name: String, start: Int, rolled: Seq[Int] = Seq.empty, score: Int = 0)

  case class Game1(player1: Player, player2: Player, pawn1: Int, pawn2: Int, dice: Dice, goal: Int)

  def play1(g: Game1): (Player,Player,Dice) =

    def won(p: Player): Boolean =
      p.score >= g.goal

    if (won(g.player2))
      (g.player2, g.player1, g.dice)
    else
      val throw1 = g.dice.roll(0)
      val throw2 = throw1.roll(0)
      val throw3 = throw2.roll(0)

      // correct mod - position is off by one
      val position = 1 + ((g.pawn1 - 1 + throw1.face + throw2.face + throw3.face) % 10)
      val played   = g.player1.copy(score = g.player1.score + position)
      play1(g.copy(
        player1 = g.player2, // swap player
        player2 = played,
        dice = throw3,
        pawn1 = g.pawn2,     // and pawns
        pawn2 = position))


  val play1 = Player(name = "#1", start = 7)
  val play2 = Player(name = "#2", start = 9)
  val game1: Game1 = Game1(play1, play2, play1.start, play2.start, CyclingDice(1, 100), 1000)

  val answer1: Int =
    val (winner, loser, dice) = play1(game1)
    val roled = dice.rolled
    val score = loser.score
    roled * score

  println(s"answer 1: ${answer1} [${System.currentTimeMillis - timestamp}ms]")
  assert(answer1 == 679329)

  case class Pawn(pos: Int, score: Int = 0):
    def move(steps: Int): Pawn =
      val npos = ((pos - 1 + steps) % 10) + 1 
      Pawn(npos, score + npos)

  val roll3x3x3 =
    val throws = for { t1 <- 1 to 3 ; t2 <- 1 to 3 ; t3 <- 1 to 3 } yield t1 + t2 + t3
    throws.groupMapReduce(identity)(_ => 1)(_ + _)

  def play2(pawn1: Pawn, pawn2: Pawn): (Long, Long) = // (winner, loser)

    def getOrTurn(pawn1: Pawn, pawn2: Pawn): (Long, Long) =
      if (pawn1.score >= 21) (1, 0) else go(pawn2, pawn1).swap

    def go(pawn1: Pawn, pawn2: Pawn): (Long, Long) =

      val turns = for {
        (roll, count) <- roll3x3x3
        npawn1         = pawn1.move(roll)
        (u1, u2)       = getOrTurn(npawn1, pawn2)
      } yield (count * u1, count * u2)

      turns.reduce { // (a, a) => a
        case ((u1a, u2a), (u1b, u2b)) => (u1a + u1b, u2a + u2b)
      }

    go(pawn1, pawn2) 

  val (score1, score2) = play2(Pawn(pos = 7), Pawn(pos = 9))

  val answer2 = score1.max(score2)
  println(s"answer 2: $answer2 [${System.currentTimeMillis - timestamp}ms]")
  assert(answer2 == 433315766324816L) // approx 2min
  