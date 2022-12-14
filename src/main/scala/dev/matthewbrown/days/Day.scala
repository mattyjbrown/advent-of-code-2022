package dev.matthewbrown.days

import cats.effect.IO
import dev.matthewbrown.Utils

import scala.util.control.NoStackTrace

trait Day {
  def day: Int

  def solve1: IO[Any] = ???

  def solve2: IO[Any] = ???
}

trait BasicDay extends Day {

  val day: Int

  type Input

  final def read: fs2.Stream[IO, String] = Utils.readLines("Day" + day.toString + ".txt")

  def parse(strs: Vector[String]): IO[Input]

  override def solve1: IO[Any] =
    for {
      strs <- read.compile.toVector
      sanitised = if strs.lastOption.exists(_.isEmpty) then strs.init else strs
      parsed <- parse(sanitised)
      answer <- solve1Impl(parsed)
    } yield answer

  def solve1Impl(input: Input): IO[Any]

  override def solve2: IO[Any] =
    for {
      strs <- read.compile.toVector
      sanitised = if strs.lastOption.exists(_.isEmpty) then strs.init else strs
      parsed <- parse(sanitised)
      answer <- solve2Impl(parsed)
    } yield answer

  def solve2Impl(input: Input): IO[Any]

}

trait DayWithDifferentParsing extends Day {

  val day: Int

  type Input1
  type Input2

  final def read: fs2.Stream[IO, String] = Utils.readLines("Day" + day.toString + ".txt")

  def parse1(strs: Vector[String]): IO[Input1]
  def parse2(strs: Vector[String]): IO[Input2]

  override def solve1: IO[Any] =
    for {
      strs <- read.compile.toVector
      sanitised = if strs.lastOption.exists(_.isEmpty) then strs.init else strs
      parsed <- parse1(sanitised)
      answer <- solve1Impl(parsed)
    } yield answer

  def solve1Impl(input: Input1): IO[Any]

  override def solve2: IO[Any] =
    for {
      strs <- read.compile.toVector
      sanitised = if strs.lastOption.exists(_.isEmpty) then strs.init else strs
      parsed <- parse2(sanitised)
      answer <- solve2Impl(parsed)
    } yield answer

  def solve2Impl(input: Input2): IO[Any]

}

case class InvalidInput(reason: String, line: String) extends NoStackTrace:
  override def getMessage: String = s"$reason ($line)"
