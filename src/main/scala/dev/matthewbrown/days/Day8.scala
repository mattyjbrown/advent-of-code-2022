package dev.matthewbrown.days

import cats.effect.IO

object Day8 extends BasicDay {
  override val day: Int = 8
  override type Input = Vector[Vector[Int]]

  override def parse(strs: Vector[String]): IO[Input] = IO {
    strs.transpose.map(_.map(_.toString.toInt))
  }

  override def solve1Impl(input: Input): IO[Any] = IO {
    (for {
      x <- input.indices
      y <- input.indices
    } yield {
      val height = input(x)(y)
      val north = input(x).take(y)
      val south = input(x).reverse.take(input.size - y - 1)
      val west = input.transpose.apply(y).take(x)
      val east = input.transpose.apply(y).reverse.take(input.size - x - 1)
      !north.exists(_ >= height) || !south.exists(_ >= height) || !east.exists(_ >= height) || !west.exists(_ >= height)
    }).count(identity)
  }

  override def solve2Impl(input: Input): IO[Any] = IO {
    extension [E](i: Seq[E])
      def takeUntil(cond: E => Boolean): Seq[E] = {
        val idx = i.indexWhere(cond)
        if idx == -1 then i else i.take(idx + 1)
      }
    (for {
      x <- input.indices
      y <- input.indices
    } yield {
      val height = input(x)(y)
      val north = input(x).take(y).reverse
      val south = input(x).reverse.take(input.size - y - 1).reverse
      val west = input.transpose.apply(y).take(x).reverse
      val east = input.transpose.apply(y).reverse.take(input.size - x - 1).reverse
      val ss = north.takeUntil(_ >= height).size * south.takeUntil(_ >= height).size * east.takeUntil(_ >= height).size * west.takeUntil(_ >= height).size
      ss
    }).max
  }
}
