import scala.io._

object Day16 extends App:

  val input: String =
    "005173980232D7F50C740109F3B9F3F0005425D36565F202012CAC0170004262EC658B0200FC3A8AB0EA5FF331201507003710004262243F8F600086C378B7152529CB4981400B202D04C00C0028048095070038C00B50028C00C50030805D3700240049210021C00810038400A400688C00C3003E605A4A19A62D3E741480261B00464C9E6A5DF3A455999C2430E0054FCBE7260084F4B37B2D60034325DE114B66A3A4012E4FFC62801069839983820061A60EE7526781E513C8050D00042E34C24898000844608F70E840198DD152262801D382460164D9BCE14CC20C179F17200812785261CE484E5D85801A59FDA64976DB504008665EB65E97C52DCAA82803B1264604D342040109E802B09E13CBC22B040154CBE53F8015796D8A4B6C50C01787B800974B413A5990400B8CA6008CE22D003992F9A2BCD421F2C9CA889802506B40159FEE0065C8A6FCF66004C695008E6F7D1693BDAEAD2993A9FEE790B62872001F54A0AC7F9B2C959535EFD4426E98CC864801029F0D935B3005E64CA8012F9AD9ACB84CC67BDBF7DF4A70086739D648BF396BFF603377389587C62211006470B68021895FCFBC249BCDF2C8200C1803D1F21DC273007E3A4148CA4008746F8630D840219B9B7C9DFFD2C9A8478CD3F9A4974401A99D65BA0BC716007FA7BFE8B6C933C8BD4A139005B1E00AC9760A73BA229A87520C017E007C679824EDC95B732C9FB04B007873BCCC94E789A18C8E399841627F6CF3C50A0174A6676199ABDA5F4F92E752E63C911ACC01793A6FB2B84D0020526FD26F6402334F935802200087C3D8DD0E0401A8CF0A23A100A0B294CCF671E00A0002110823D4231007A0D4198EC40181E802924D3272BE70BD3D4C8A100A613B6AFB7481668024200D4188C108C401D89716A080"

  sealed trait Bit:
    override def toString = this match { case O => "0" ; case I => "1" }
  case object O extends Bit
  case object I extends Bit

  type Version = Int       // 3 lsb
  type Id      = Int       // 3 lsb
  type Word    = List[Bit] // grouped 4 lsb

  case class Group(last: Boolean, word: Word)
  object Group:
    def apply(bits: List[Bit]): Group =
      Group(isLast(bits), bits.drop(1))
    def isLast(bits: List[Bit]): Boolean =
      bits.head == O


  sealed trait Packet:
    val version: Version
    val id: Id
    val subs: List[Packet]

    def value: Long
    def versionSum: Int = version + subs.map(_.versionSum).sum

  case class Lit(override val version: Version, override val id: Id, override val value: Long, override val subs: List[Packet] = List.empty) extends Packet
    

  case class Sum(override val version: Version, override val id: Id, override val subs: List[Packet]) extends Packet:
    def value: Long = subs.map(_.value).sum

  case class Product(override val version: Version, override val id: Id, override val subs: List[Packet]) extends Packet:
    def value: Long = subs.map(_.value).product
  case class Min(override val version: Version, override val id: Id, override val subs: List[Packet]) extends Packet:
    def value: Long = subs.map(_.value).min
  case class Max(override val version: Version, override val id: Id, override val subs: List[Packet]) extends Packet:
    def value: Long = subs.map(_.value).max
  case class Gt(override val version: Version, override val id: Id, override val subs: List[Packet]) extends Packet:
    assert(subs.size == 2)
    def value: Long = if (subs(0).value > subs(1).value) 1 else 0
  case class Lt(override val version: Version, override val id: Id, override val subs: List[Packet]) extends Packet:
    assert(subs.size == 2)
    def value: Long = if (subs(0).value < subs(1).value) 1 else 0
  case class Eq(override val version: Version, override val id: Id, override val subs: List[Packet]) extends Packet:
    assert(subs.size == 2)
    def value: Long = if (subs(0).value == subs(1).value) 1 else 0

  object Packet:
    def apply(input: String): Packet =
      def unwrap(bits: String, idx: Int = 0, acc: List[Bit] = List.empty): List[Bit] =
        if (idx >= 8) acc
        else unwrap(bits.tail, idx + 1, acc :+ (bits.head match { case '0' => O ; case '1' => I }))

      parse(input
        .grouped(2)
        .map(hex => String.format("%8s", Integer.parseInt(hex, 16).toInt.toBinaryString).replace(' ', '0'))
        .flatMap(bytes => unwrap(bytes))
        .toList)._1

    def parse(bits: List[Bit]): (Packet, List[Bit]) =
      header(bits) match {
        case (version, 4, rest)  =>
          val (gs, tail) = groups(rest)
          (Lit(version, 4, long(gs.foldLeft(List.empty)((a,g) => a ++ g.word))), tail)

        case (version, id, rest) if rest.head == O =>
          val length  = rest.drop(1).take(15)
          val chunked = rest.drop(16).take(int(length))
          val (ps, _) = subs(chunked, None)
          (opp(version, id, ps), rest.drop(16 + int(length)))

        case (version, id, rest) if rest.head == I =>
          val length = rest.drop(1).take(11)
          val (ps, tail) = subs(rest.drop(12), Some(int(length)))
          (opp(version, id, ps), tail)
          
        case (_, _, _) =>
          sys.error("boom!")
      }

    def groups(rest: List[Bit]): (List[Group], List[Bit]) =
      def go(groups: List[List[Bit]], acc: List[Group] = List.empty): (List[Group], List[Bit]) =
        groups match
          case Nil => sys.error("no last group")
          case bits :: rest if Group.isLast(bits) => (acc :+ Group(bits), rest.flatten)
          case bits :: rest => go(rest, acc :+ Group(bits))
      go(rest.grouped(5).toList)

    def subs(bs: List[Bit], stop: Option[Int], acc: List[Packet] = List.empty): (List[Packet], List[Bit]) =
      if (bs.isEmpty || stop.map(_ == acc.size).getOrElse(false))
        (acc, bs)
      else
        val (p, r) = parse(bs)
        subs(r, stop, acc :+ p)

    def opp(version: Version, id: Id, subs: List[Packet]): Packet =
      id match
        case 0 => Sum(version, id, subs)
        case 1 => Product(version, id, subs)
        case 2 => Min(version, id, subs)
        case 3 => Max(version, id, subs)
        case 5 => Gt(version, id, subs)
        case 6 => Lt(version, id, subs)
        case 7 => Eq(version, id, subs)

    def long(bits: List[Bit]): Long =
      java.lang.Long.parseLong(bits.map(_.toString).mkString(""), 2).toLong
      
    def int(bits: List[Bit]): Int =
      long(bits).toInt

    def header(bits: List[Bit]): (Version, Id, List[Bit]) =
      (int(bits.take(3)), int(bits.drop(3).take(3)), bits.drop(6))

    def str(bits: List[Bit]): String =
      bits.foldLeft("")((a,b) => a + b)

  val start = System.currentTimeMillis
  println(s"Answer 1 = ${Packet(input).versionSum} [${System.currentTimeMillis - start}ms]")
  println(s"Answer 2 = ${Packet(input).value} [${System.currentTimeMillis - start}ms]")
