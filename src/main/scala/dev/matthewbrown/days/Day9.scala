package dev.matthewbrown.days

import cats.data.NonEmptyList
import cats.effect.IO
import dev.matthewbrown.Utils.IntExtractor

object Day9 extends BasicDay {
  override val day: Int = 9
  override type Input = Vector[Direction]

  case class Cell(x: Int, y: Int) {
    def move(v: Vec): Cell = Cell(x + v.x, y + v.y)
    def minus(other: Cell): Vec = Vec(x - other.x, y - other.y)

    def isNeighbourOrSame(other: Cell): Boolean =
      other == this ||
        (other.x - x).abs <= 1 && (other.y - y).abs <= 1
  }
  case class Vec(x: Int, y: Int) {
    def clamp: Vec = Vec(Math.max(-1, Math.min(x, 1)), Math.max(-1, Math.min(y, 1)))
  }
  enum Direction(val unit: Vec):
    case L extends Direction(Vec(-1,0))
    case R extends Direction(Vec(1,0))
    case U extends Direction(Vec(0,1))
    case D extends Direction(Vec(0,-1))

  object Direction {
    def unapply(str: String): Option[Direction] = str match
      case "L" => Some(Direction.L)
      case "R" => Some(Direction.R)
      case "U" => Some(Direction.U)
      case "D" => Some(Direction.D)
      case _ => None
  }

  override def parse(strs: Vector[String]): IO[Input] = IO {
    strs.flatMap(s => s.split(' ') match
      case Array(Direction(d), IntExtractor(i)) => List.fill(i)(d)
      case _ => throw InvalidInput("wut", s)
    )
  }

  case class State(heads: NonEmptyList[Cell], tails: NonEmptyList[Cell]) {
    def head: Cell = heads.head
    def tail: Cell = tails.head
    def proc(dir: Direction): State = {
      val newHead = head.move(dir.unit)
      val newTail =
        if tail.isNeighbourOrSame(newHead) then tail
        else tail.move(newHead.minus(tail).clamp)

      State(newHead :: heads, newTail :: tails)
    }
  }

  object State {
    val init = State(NonEmptyList.one(Cell(0,0)), NonEmptyList.one(Cell(0,0)))
  }

  override def solve1Impl(input: Input): IO[Any] = IO {
    input.foldLeft(State2.init(2))(_.proc(_))
  }.map(_.knots.last.toList.distinct.size)

  case class State2(knots: List[NonEmptyList[Cell]]) {
    extension (knot: NonEmptyList[Cell]) {
      def move(dir: Direction): NonEmptyList[Cell] = {
        knot.head.move(dir.unit) :: knot
      }
      def follow(cell: Cell): NonEmptyList[Cell] = {
        (if knot.head.isNeighbourOrSame(cell) then knot.head
        else knot.head.move(cell.minus(knot.head).clamp)) :: knot
      }
    }

    /**
     * Fold down the list of knots to rebuild it
     * Each proc takes the LAST of the acc list of knots, and that.head will be the knot it's following
     */
    def proc(dir: Direction): State2 = State2 {
      knots match
        case heads :: rests => rests.foldLeft(List(heads.move(dir)))((newKnots, oldKnot) =>
          newKnots.appended(oldKnot.follow(newKnots.last.head))
        )
        case Nil => Nil
    }
  }

  object State2 {
    def init(ropeSize: Int): State2 = State2(List.fill(ropeSize)(NonEmptyList.one(Cell(0,0))))
  }
  override def solve2Impl(input: Input): IO[Any] = IO {
    input.foldLeft(State2.init(10))(_.proc(_))
  }.map(_.knots.last.toList.distinct.size)
}
