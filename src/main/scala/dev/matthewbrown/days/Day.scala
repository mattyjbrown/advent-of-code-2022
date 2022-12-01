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
      parsed <- parse(strs)
      answer <- solve1Impl(parsed)
    } yield answer

  def solve1Impl(input: Input): IO[String]

  override def solve2: IO[String] =
    for {
      strs <- read.compile.toVector
      parsed <- parse(strs)
      answer <- solve2Impl(parsed)
    } yield answer

  def solve2Impl(input: Input): IO[String]

}

case class InvalidInput(reason: String, line: String) extends NoStackTrace
