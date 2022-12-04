package dev.matthewbrown

import cats.data.NonEmptyList
import cats.effect.IOApp
import cats.effect.IO
import dev.matthewbrown.days.*

object Main extends IOApp.Simple {

    override def run: IO[Unit] = {
      val day: Day = Day4
      for {
        _ <- IO.println("Hello, World!")
        pt1 <- day.solve1
        _ <- IO.println(s"Day ${day.day}, Part 1: $pt1")
        pt2 <- day.solve2
        _ <- IO.println(s"Day ${day.day}, Part 2: $pt2")
      } yield ()
    }
    
}
