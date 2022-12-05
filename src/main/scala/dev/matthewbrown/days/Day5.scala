package dev.matthewbrown.days

import cats.effect.IO
import dev.matthewbrown.Utils.IntExtractor

object Day5 extends BasicDay {
  override val day: Int = 5
  override type Input = (Vector[Vector[Char]], Vector[Step])

  case class Step(count: Int, from: Int, to: Int)
  object Step {
    def parse(str: String): Step =
      str.split(' ') match
        case Array("move", IntExtractor(count), "from", IntExtractor(from), "to", IntExtractor(to)) => Step(count, from, to)
        case _ => throw InvalidInput("wut", str)
  }
  override def parse(strs: Vector[String]): IO[Input] = IO {
    val drawing = strs
      .takeWhile(_.nonEmpty)

    val steps = strs.drop(drawing.length + 1).map(Step.parse)

    drawing.map(_.padTo(strs.maxBy(_.length).length, ' ')).transpose
      .map(_.filterNot(c => c == ' ' || c == '[' || c == ']'))
      .filterNot(_.isEmpty)
      .map(_.init) -> steps
  }

  override def solve1Impl(input: Input): IO[Any] = IO {
    val (drawing, steps) = input
    steps.foldLeft(drawing){(state, step) =>
      val fromIdx = step.from - 1
      val toMove = state(fromIdx).take(step.count).reverse
      val toIdx = step.to - 1
      state.updated(fromIdx, state(fromIdx).drop(step.count)).updated(toIdx, state(toIdx).prependedAll(toMove))
    }
  }.map(_.map(_.head).mkString)

  override def solve2Impl(input: Input): IO[Any] = IO {
    val (drawing, steps) = input
    steps.foldLeft(drawing) { (state, step) =>
      val fromIdx = step.from - 1
      val toMove = state(fromIdx).take(step.count)
      val toIdx = step.to - 1
      state.updated(fromIdx, state(fromIdx).drop(step.count)).updated(toIdx, state(toIdx).prependedAll(toMove))
    }
  }.map(_.map(_.head).mkString)
}
