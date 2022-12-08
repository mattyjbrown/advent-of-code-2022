package dev.matthewbrown.days

import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits.*
import dev.matthewbrown.Utils.IntExtractor
import dev.matthewbrown.days.Day7.Line.{Command, Output}
import dev.matthewbrown.days.Day7.Line.Command.CDTarget

object Day7 extends BasicDay {
  sealed trait Line

  object Line {
    sealed trait Command extends Line

    object Command {
      sealed trait CDTarget

      object CDTarget {
        case object Root extends CDTarget

        case object Up extends CDTarget

        case class Down(dir: String) extends CDTarget

        def parse(str: String): Option[CDTarget] = str match
          case "/" => Root.some
          case ".." => Up.some
          case other => Down(other).some

        def unapply(str: String): Option[CDTarget] = parse(str)
      }

      case class CD(target: CDTarget) extends Command

      case object LS extends Command

      def parse(str: String): Option[Command] = str.split(' ') match
        case Array("$", "cd", CDTarget(target)) => CD(target).some
        case Array("$", "ls") => LS.some
    }

    sealed trait Output extends Line

    object Output {
      enum FSObject extends Output:
        case File(size: Int, name: String)
        case Dir(name: String)

      def parse(str: String): Option[Output] = str.split(' ') match
        case Array("dir", name) => FSObject.Dir(name).some
        case Array(IntExtractor(size), name) => FSObject.File(size, name).some
        case _ => None
    }

    def parse(str: String): Option[Line] =
      if str.startsWith("$ ")
      then Command.parse(str)
      else Output.parse(str)
  }

  /**
   * A path is a List[String].
   * A file's path is a NonEmptyList[String], because path.head is the name
   * A file system is just a List[File], where the dirs are emergent features
   */
  case class File(path: NonEmptyList[String], size: Int)
  case class ParseResult(files: List[File], location: List[String]) {
    private def cd(target: CDTarget): List[String] = target match
      case CDTarget.Root => Nil
      case CDTarget.Up => location.tail //Will throw if you .. at /. I wish Nil.tail == Nil
      case CDTarget.Down(dir) => dir :: location

    def process(line: Line): ParseResult = line match
      case Line.Command.CD(target) => copy(location = cd(target))
      case Line.Command.LS => this //Nothing to do, just getting info out the system
      case Line.Output.FSObject.Dir(name) => this//Nothing to do, I don't do dirs
      case Line.Output.FSObject.File(size, name) => copy(files :+ File(NonEmptyList(name, location), size))
  }

  override val day: Int = 7
  override type Input = List[File]

  extension (files: List[File]) {
    def dirs: List[List[String]] = files.flatMap(_.path.toList.tails).distinct
    def dirSize(dir: List[String]): Int = files.collect {
      case File(path, size) if path.tail.endsWith(dir) => size
    }.sum
  }
  extension (path: List[String]) {
    def printStr: String = path.reverse.mkString("/", "/", "")
  }

  override def parse(strs: Vector[String]): IO[Input] = IO {
    val (init, rest) = strs.map(s => Line.parse(s).getOrElse(throw InvalidInput("not a line", s))).toList match
      case a :: rest if a == Line.Command.CD(CDTarget.Root) => ParseResult(Nil, Nil) -> rest
      case other => throw InvalidInput("invalid first lines?", other.take(2).mkString("|"))

    rest.foldLeft(init)((state, line) => state.process(line)).files
  }

  override def solve1Impl(input: Input): IO[Any] = IO {
    input.dirs.map(input.dirSize).filter(_ <= 100000).sum
  }

  override def solve2Impl(input: Input): IO[Any] = IO {
    val taken = input.dirSize(Nil)
    val free = 70000000 - taken
    val needed = 30000000
    val toFree = -(free - needed)
    input.dirs.map(input.dirSize).filter(_ >= toFree).minOption
  }
}
