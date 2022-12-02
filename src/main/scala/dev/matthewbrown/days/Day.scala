package dev.matthewbrown.days

import cats.effect.IO
import dev.matthewbrown.Utils

import scala.util.control.NoStackTrace

trait Day {
  def day: Int

  def solve1: IO[String] = ???

  def solve2: IO[String] = ???
}

trait BasicDay extends Day {

  val day: Int

  type Input

  final def read: fs2.Stream[IO, String] = Utils.readLines("Day" + day.toString + ".txt")

  def parse(strs: Vector[String]): IO[Input]

  override def solve1: IO[String] =
    for {
      strs <- read.compile.toVector
      sanitised = if (strs.last.isEmpty) strs.init else strs
      parsed <- parse(sanitised)
      answer <- solve1Impl(parsed)
    } yield answer

  def solve1Impl(input: Input): IO[String]

  override def solve2: IO[String] =
    for {
      strs <- read.compile.toVector
      sanitised = if (strs.last.isEmpty) strs.init else strs
      parsed <- parse(sanitised)
      answer <- solve2Impl(parsed)
    } yield answer

  def solve2Impl(input: Input): IO[String]

}

trait DayWithDifferentParsing extends Day {

  val day: Int

  type Input1
  type Input2

  final def read: fs2.Stream[IO, String] = Utils.readLines("Day" + day.toString + ".txt")

  def parse1(strs: Vector[String]): IO[Input1]
  def parse2(strs: Vector[String]): IO[Input2]

  override def solve1: IO[String] =
    for {
      strs <- read.compile.toVector
      sanitised = if (strs.last.isEmpty) strs.init else strs
      parsed <- parse1(sanitised)
      answer <- solve1Impl(parsed)
    } yield answer

  def solve1Impl(input: Input1): IO[String]

  override def solve2: IO[String] =
    for {
      strs <- read.compile.toVector
      sanitised = if (strs.last.isEmpty) strs.init else strs
      parsed <- parse2(sanitised)
      answer <- solve2Impl(parsed)
    } yield answer

  def solve2Impl(input: Input2): IO[String]

}

case class InvalidInput(reason: String, line: String) extends NoStackTrace:
  override def getMessage: String = s"$reason ($line)"
