package dev.matthewbrown.days

import cats.effect.IO
import dev.matthewbrown.days.Day13.Value.{VInt, VList}
import io.circe.Json

import scala.util.{Failure, Success, Try}

object Day13 extends BasicDay {

  enum Trilean:
    case True
    case False
    case Undetermined

  override val day: Int = 13
  override type Input = Vector[(Value, Value)]

  sealed trait Value { left =>
    override def toString: String = this match
      case Value.VInt(int) => int.toString
      case Value.VList(list) => list.mkString("[", ",", "]")

    def compare(right: Value): Trilean =
      (left, right) match
        case (VInt(li), VInt(ri)) => if li < ri then Trilean.True else if li > ri then Trilean.False else /* if li == ri then */ Trilean.Undetermined
        case (ll: VList, ri: VInt) => ll.compare(VList(List(ri)))
        case (li: VInt, rl: VList) => VList(List(li)).compare(rl)
        case (VList(ll), VList(rl)) => (ll.headOption, rl.headOption) match
          case (Some(l), Some(r)) => l.compare(r) match
            case Trilean.True => Trilean.True
            case Trilean.False => Trilean.False
            case Trilean.Undetermined => VList(ll.tail).compare(VList(rl.tail))
          case (None, Some(r)) => Trilean.True
          case (Some(l), None) => Trilean.False
          case (None, None) => Trilean.Undetermined
  }

  object Value {
    case class VInt(int: Int) extends Value

    case class VList(list: List[Value]) extends Value

    /**
     * I'm pretty sure using circe is cheating...
     */
    def parseFromJsonStr(jsonStr: String): Value = {
      io.circe.parser.parse(jsonStr).map(parseFromJson).getOrElse(throw InvalidInput(s"Line is not Json", jsonStr))
    }

    def parseFromJson(json: Json): Value =
      json.asNumber.flatMap(_.toInt).map(VInt.apply).orElse(json.asArray.map(_.map(parseFromJson).toList).map(VList.apply))
        .getOrElse(throw InvalidInput(s"Json is not a Value", json.noSpaces))

   //def tokenise(str: String): Array[String] =
   //  if str.isEmpty then Array.empty else {
   //    val untilFirstList = str.takeWhile(_ != '[')
   //    val firstList = str.slice(untilFirstList.length, str.lastIndexOf(']') + 1) //TODO: WRONG. You want the NEXT ] but then add one on for each time you see a [ on the way?
   //    val rest = str.drop(untilFirstList.length).drop(firstList.length).drop(1) //The last 1 is dropping the comma
   //    (untilFirstList.split(',') ++ Option.when(firstList.nonEmpty)(firstList) ++ tokenise(rest))
   //      .filter(_.nonEmpty)
   //  }

   //def parse(str: String): Value =
   //  if str.toIntOption.isDefined
   //  then VInt.apply(str.toInt)
   //  else if str.startsWith("[") && str.endsWith("]") then {
   //    val inside = str.tail.init
    //
    //  }
    //  else throw InvalidInput("Wut", str)
  }

  override def parse(strs: Vector[String]): IO[Input] = IO {
    strs.grouped(3).map(v => v(0) -> v(1)).map { (line1, line2) =>
      val v1 = Try(Value.parseFromJsonStr(line1)) match
        case Failure(exception) => throw InvalidInput(s"Got $exception", line1)
        case Success(value) => value
      val v2 = Try(Value.parseFromJsonStr(line2)) match
        case Failure(exception) => throw InvalidInput(s"Got $exception", line2)
        case Success(value) => value
      v1 -> v2
    }.toVector
  }

  override def solve1Impl(input: Input): IO[Any] = IO {
    input.zipWithIndex.map { case ((rv, lv), i) =>
      rv.compare(lv) match
        case Trilean.True =>
          println(s"Right order {$rv} v {$lv}. ${i + 1}")
          i + 1
        case Trilean.False =>
          println(s"Wrong order {$rv} v {$lv}. 0")
          0
        case Trilean.Undetermined =>
          println(s"OOFT. Undetermined:  {$rv} v {$lv}. Will assume right. ${i + 1}")
          i + 1
    }.sum
  }

  private val dividerPackets = Vector(
    VList(List(VList(List(VInt(2))))),
    VList(List(VList(List(VInt(6))))),
  )
  override def solve2Impl(input: Input): IO[Any] = IO {
    val sorted = (input.flatMap(Vector(_, _)) ++ dividerPackets).sortWith(_.compare(_) match
      case Trilean.True => true
      case Trilean.False => false
      case Trilean.Undetermined => println(s"OOFT. Undetermined. Will assume right."); true
    )
    
    sorted.zipWithIndex.collect {
      case (value, i) if dividerPackets.contains(value) => i + 1
    }.product
  }
}
