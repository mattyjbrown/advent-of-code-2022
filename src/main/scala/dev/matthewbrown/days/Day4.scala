package dev.matthewbrown.days
import cats.effect.IO

object Day4 extends BasicDay {
  override val day: Int = 4
  override type Input = Vector[(Range, Range)]

  extension (r: Range.type) {
    def unapply(str: String): Option[Range] = str.split('-') match
      case Array(one, two) if one.toIntOption.isDefined && two.toIntOption.isDefined => Some(r.inclusive(one.toInt, two.toInt))
      case _ => None
  }

  override def parse(strs: Vector[String]): IO[Input] = IO {
    strs.map { line =>
      line.split(',') match
        case Array(Range(r1), Range(r2)) => r1 -> r2
        case _ => throw InvalidInput("not two ranges after split", line)
    }
  }

  override def solve1Impl(input: Input): IO[Any] = IO {
    input.count((r1, r2) => r1.containsSlice(r2) || r2.containsSlice(r1))
  }

  override def solve2Impl(input: Input): IO[Any] = IO {
    input.count((r1, r2) => r1.intersect(r2).nonEmpty)
  }
}
