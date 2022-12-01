package dev.matthewbrown.days

import cats.data.NonEmptyList
import cats.effect.IO

object Day1 extends BasicDay {
  override val day: Int = 1

  override type Input = Vector[Option[Int]]

  override def parse(strs: Vector[String]): IO[Input] = IO {
    strs.map {
      case "" => None
      case integer if integer.toIntOption.isDefined => integer.toIntOption
      case other => throw InvalidInput("Not an Int", other)
    }
  }

  override def solve1Impl(input: Input): IO[String] = IO {
    input.foldLeft(NonEmptyList.one(0)) { (acc, line) =>
      line match
        case Some(calories) => NonEmptyList(acc.head + calories, acc.tail)
        case None => acc.prepend(0)
    }
  }.map(_.toList.max.toString)

  override def solve2Impl(input: Input): IO[String] = IO {
    input.foldLeft(NonEmptyList.one(0)) { (acc, line) =>
      line match
        case Some(calories) => NonEmptyList(acc.head + calories, acc.tail)
        case None => acc.prepend(0)
    }
  }.map(_.toList.sorted.reverse.take(3).sum.toString)
}
