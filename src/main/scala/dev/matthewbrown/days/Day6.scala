package dev.matthewbrown.days

import cats.effect.IO

object Day6 extends BasicDay {
  override val day: Int = 6
  override type Input = String

  override def parse(strs: Vector[String]): IO[Input] = IO {
    strs.mkString
  }

  override def solve1Impl(input: Input): IO[Any] = IO {
    input.sliding(4).zipWithIndex.collectFirst {
      case (str, i) if str.toSet.size == 4 => i + 4
    }
  }

  override def solve2Impl(input: Input): IO[Any] = IO {
    input.sliding(14).zipWithIndex.collectFirst {
      case (str, i) if str.toSet.size == 14 => i + 14
    }
  }
}
