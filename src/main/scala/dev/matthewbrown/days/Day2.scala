package dev.matthewbrown.days

import cats.data.NonEmptyList
import cats.effect.IO
import dev.matthewbrown.days.Day2.Choice.{Rock, Scissors}

object Day2 extends DayWithDifferentParsing {
  override val day: Int = 2
  
  enum Outcome(val points: Int):
    case Win extends Outcome(6)
    case Lose extends Outcome(0)
    case Draw extends Outcome(3)

  object Outcome:
    def parse(str: String): Option[Outcome] = str match
      case "X" => Some(Outcome.Lose)
      case "Y" => Some(Outcome.Draw)
      case "Z" => Some(Outcome.Win)
      case _ => None

    def unapply(char: Char): Option[Outcome] = parse(char.toString)

  enum Choice(val points: Int):
    final def beats: Choice = this match
      case Rock => Scissors
      case Scissors => Paper
      case Paper => Rock
      
    final def losesTo: Choice = this match
      case Rock => Paper
      case Scissors => Rock
      case Paper => Scissors

    final def outcome(other: Choice): Outcome =
      if this.beats == other
        then Outcome.Win
      else if other.beats == this
        then Outcome.Lose
      else
        Outcome.Draw

    case Rock extends Choice(1)
    case Scissors extends Choice(3)
    case Paper extends Choice(2)

  object Choice:
    def parse(str: String): Option[Choice] = str match
      case "A" | "X" => Some(Choice.Rock)
      case "B" | "Y" => Some(Choice.Paper)
      case "C" | "Z" => Some(Choice.Scissors)
      case _ => None
    def unapply(char: Char): Option[Choice] = parse(char.toString)
  end Choice

  override type Input1 = Vector[(Choice, Choice)]
  override type Input2 = Vector[(Choice, Outcome)]

  override def parse1(strs: Vector[String]): IO[Input1] = IO {
    strs.map(_.toList).map {
      case Choice(first) :: ' ' :: Choice(second) :: Nil => first -> second
      case other => throw InvalidInput("Not a valid row", other.mkString)
    }
  }

  override def parse2(strs: Vector[String]): IO[Input2] = IO {
    strs.map(_.toList).map {
      case Choice(first) :: ' ' :: Outcome(second) :: Nil => first -> second
      case other => throw InvalidInput("Not a valid row", other.mkString)
    }
  }

  override def solve1Impl(input: Input1): IO[String] = IO {
    input
      .map((other, you) => you -> you.outcome(other))
      .map((choice, result) => choice.points + result.points)
      .sum
  }.map(_.toString)

  override def solve2Impl(input: Input2): IO[String] = IO {
    input.map((other, result) => other -> (result match
      case Outcome.Win => other.losesTo
      case Outcome.Draw => other
      case Outcome.Lose => other.beats
    ))
      .map((other, you) => you -> you.outcome(other))
      .map((choice, result) => choice.points + result.points)
      .sum
  }.map(_.toString)
}
