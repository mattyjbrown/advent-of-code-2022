package dev.matthewbrown.days

import cats.effect.IO
import dev.matthewbrown.Utils.IntExtractor

import scala.util.Try

object Day11 extends BasicDay {
  override val day: Int = 11
  override type Input = Map[Int, Monkey]

  enum Arithmetic:
    case Add
    case Minus
    case Multiply
    case Divide

    def apply(a: BigInt, b: BigInt): BigInt = this match
      case Add => a + b
      case Minus => a - b
      case Multiply => a * b
      case Divide => a / b

  object Arithmetic {
    def unapply(str: String): Option[Arithmetic] = str match
      case "+" => Some(Arithmetic.Add)
      case "-" => Some(Arithmetic.Minus)
      case "*" => Some(Arithmetic.Multiply)
      case "/" => Some(Arithmetic.Divide)
      case _ => None
  }

  enum Target:
    case Old
    case Number(i: Int)

  object Target {
    def unapply(str: String): Option[Target] = str match
      case "old" => Some(Old)
      case IntExtractor(i) => Some(Number(i))
      case _ => None
  }

  case class Operation(op: Arithmetic, t: Target) {
    def apply(old: BigInt): BigInt = op.apply(old, t match
      case Target.Old => old
      case Target.Number(i) => i
    )
  }

  object Operation {
    def parse(str: String): Operation = str.trim.stripPrefix("Operation: new = ").split(' ') match
      case Array("old", Arithmetic(arithmetic), Target(target)) => Operation(arithmetic, target)
      case _ => throw InvalidInput(s"not an Operation", str)
  }


  def parseTestNo(str: String): Int = str.trim.stripPrefix("Test: divisible by ").toInt
  def parseIfTrue(str: String): Int = str.trim.stripPrefix("If true: throw to monkey ").toInt
  def parseIfFalse(str: String): Int = str.trim.stripPrefix("If false: throw to monkey ").toInt

  case class Monkey(items: Vector[BigInt], operation: Operation, testNo: Int, monkeyIfTrue: Int, monkeyIfFalse: Int, inspectionCount: BigInt) {
    /**
     * Returns None if out of items
     * Returns Some(monkey -> worry number) otherwise
     */
    def sim(divBy3: Boolean): Option[(Int, BigInt)] = items.headOption.map { worry =>
      val newWorry = if divBy3 then operation(worry) / 3 else operation(worry)
      val target = if newWorry % testNo == 0 then monkeyIfTrue else monkeyIfFalse
      target -> newWorry
    }
  }

  override def parse(strs: Vector[String]): IO[Input] = IO {
    strs.grouped(7).zipWithIndex.map { (monkey, number) =>
      val starting = Try(monkey(1).trim.stripPrefix("Starting items: ").split(", ").map(_.toInt)).getOrElse(throw InvalidInput(s"Failed to parse the starting", s"${monkey.applyOrElse(1, "???")}"))
      val operation = Try(Operation.parse(monkey(2))).getOrElse(throw InvalidInput(s"Failed to parse the operation", s"${monkey.applyOrElse(2, "???")}"))
      val test = Try(parseTestNo(monkey(3))).getOrElse(throw InvalidInput(s"Failed to parse the test", s"${monkey.applyOrElse(3, "???")}"))
      val ifTrue = Try(parseIfTrue(monkey(4))).getOrElse(throw InvalidInput(s"Failed to parse the ifTrue", s"${monkey.applyOrElse(4, "???")}"))
      val ifFalse = Try(parseIfFalse(monkey(5))).getOrElse(throw InvalidInput(s"Failed to parse the ifFalse", s"${monkey.applyOrElse(5, "???")}"))
      number -> Monkey(starting.toVector.map(BigInt.apply), operation, test, ifTrue, ifFalse, 0)
    }.toMap
  }

  override def solve1Impl(input: Input): IO[Any] = IO {
    val indices = input.keySet.toList.sorted
    def round(input: Input): Input = {
      val res = indices.foldLeft(input) { (stateForRound, thisMonkey) =>
        val monkey = stateForRound(thisMonkey)

        def simUntil(monkey: Monkey, acc: List[(Int, BigInt)]): List[(Int, BigInt)] =
          monkey.sim(divBy3 = true) match
            case Some(tuple) => simUntil(monkey.copy(items = monkey.items.tail), tuple :: acc)
            case None => acc

        val res = simUntil(monkey, Nil).reverse.groupMap(_._1)(_._2).withDefaultValue(Vector.empty)
        indices.foldLeft(stateForRound) { (stateForMonkey, nextMonkey) =>
          stateForMonkey.updatedWith(nextMonkey) {
            case Some(monkey) =>
              if thisMonkey == nextMonkey
              then Some(monkey.copy(items = Vector.empty, inspectionCount = monkey.inspectionCount + monkey.items.size))
              else Some(monkey.copy(items = monkey.items.appendedAll(res(nextMonkey))))
            case None => throw new RuntimeException(s"PANIC $nextMonkey")
          }
        }
      }
      res
    }
    val res = (1 to 20).foldLeft(input)((state, _) => round(state)).values.toList.map(_.inspectionCount)
    res.sorted.reverse.take(2).foldLeft(BigInt(1))(_ * _)
  }

  override def solve2Impl(input: Input): IO[Any] = IO {
    val indices = input.keySet.toList.sorted
    val bigDiv = input.foldLeft(1)(_ * _._2.testNo)

    def round(input: Input): Input = {
      val res = indices.foldLeft(input) { (stateForRound, thisMonkey) =>
        val monkey = stateForRound(thisMonkey)

        def simUntil(monkey: Monkey, acc: List[(Int, BigInt)]): List[(Int, BigInt)] =
          monkey.sim(divBy3 = false) match
            case Some(tuple) => simUntil(monkey.copy(items = monkey.items.tail), tuple :: acc)
            case None => acc

        val res = simUntil(monkey, Nil).reverse.groupMap(_._1)(_._2).withDefaultValue(Vector.empty)
        indices.foldLeft(stateForRound) { (stateForMonkey, nextMonkey) =>
          stateForMonkey.updatedWith(nextMonkey) {
            case Some(monkey) =>
              if thisMonkey == nextMonkey
              then Some(monkey.copy(items = Vector.empty, inspectionCount = monkey.inspectionCount + monkey.items.size))
              else Some(monkey.copy(items = monkey.items.appendedAll(res(nextMonkey))))
            case None => throw new RuntimeException(s"PANIC $nextMonkey")
          }
        }
      }
      res.view.mapValues(m => m.copy(items = m.items.map(_ % bigDiv))).toMap
    }

    val res = (1 to 10000).foldLeft(input)((state, _) => round(state)).values.toList.map(_.inspectionCount)
    res.sorted.reverse.take(2).foldLeft(BigInt(1))(_ * _)
  }
}
