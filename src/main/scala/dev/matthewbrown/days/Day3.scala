package dev.matthewbrown.days

import cats.data.NonEmptyList
import cats.effect.IO

object Day3 extends DayWithDifferentParsing {

  case class Rucksack(first: String, second: String)

  type UpToThree[+T] = T | (T, T) | (T, T, T)

  extension (c: Char)
    def priority: Int =
      if c.isUpper then c - 38
      else if c.isLower then c - 96
      else throw InvalidInput("Is not a letter", c.toString)

  override val day: Int = 3

  override type Input1 = Vector[Rucksack]
  override type Input2 = Vector[(Rucksack, Rucksack, Rucksack)]

  override def parse1(strs: Vector[String]): IO[Input1] = IO {
    if strs.forall(_.forall(_.isLetter))
    then strs.map(row => row.splitAt(row.length / 2)).map(Rucksack.apply)
    else throw InvalidInput("At least one invalid character", strs.find(_.exists(!_.isLetter)).getOrElse("???"))

  }

  override def parse2(strs: Vector[String]): IO[Input2] = parse1(strs).map { listOfRucksacks =>
    val empty: List[UpToThree[Rucksack]] = Nil
    listOfRucksacks.foldLeft(empty) { case (acc, next) => acc match
      case Nil => next :: Nil
      case (one: Rucksack) :: tail => (one, next) :: tail
      case (one, two) :: tail => (one, two, next) :: tail
      case (one, two, three) :: tail => next :: (one, two, three) :: tail
    }.collect {
      case (one, two, three) => (one, two, three)
    }.toVector
  }

  override def solve1Impl(input: Input1): IO[Any] = IO {
    input.map(r => r.first.intersect(r.second).distinct)
      .mkString
      .map(_.priority)
      .sum
  }

  override def solve2Impl(input: Input2): IO[Any] = IO {
    input.map { case (Rucksack(f1, s1), Rucksack(f2, s2), Rucksack(f3, s3)) =>
      (f1 + s1).intersect(f2 + s2).intersect(f3 + s3).distinct
    }.mkString
      .map(_.priority)
      .sum
  }
}
