package dev.matthewbrown

import cats.effect.kernel.{Async, Concurrent, Resource}
import cats.effect.std.Console
import cats.implicits.*
import fs2.io.file.{Files, Flags, Path}
import fs2.{Pipe, Stream, text}

object Utils {

  def readLines[F[_] : Files](path: String): Stream[F, String] =
    Files[F]
      .readAll(Path(s"src/main/resources/" + path))
      .through(text.utf8.decode)
      .through(text.lines)
      .map(_.trim)
}
