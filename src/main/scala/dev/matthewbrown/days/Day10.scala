package dev.matthewbrown.days

import cats.data.Writer
import cats.effect.{IO, Sync}
import cats.implicits.*
import dev.matthewbrown.Utils.IntExtractor
import dev.matthewbrown.days.Day10.CPU.init

import scala.collection.mutable.ListBuffer

object Day10 extends BasicDay {
  override val day: Int = 10

  enum Instruction:
    case Noop
    case Addx(i: Int)

    def duration: Int = this match
      case Noop => 1
      case Addx(_) => 2

  object Instruction:
    def parse(str: String): Option[Instruction] = str.split(' ') match
      case Array("noop") => Some(Instruction.Noop)
      case Array("addx", IntExtractor(i)) => Some(Instruction.Addx(i))
      case _ => None

  override type Input = List[Instruction]

  override def parse(strs: Vector[String]): IO[Input] = IO {
    strs.map(s => Instruction.parse(s).getOrElse(throw InvalidInput("Not an instruction", s))).toList
  }

  enum State:
    case Processing(instruction: Instruction, remaining: Int)
    case Finished

  case class CPU(register: Int, state: State, instructions: List[Instruction], cycle: Int) {
    def next: CPU = state match
      case State.Processing(instruction, remaining) =>
        if remaining > 1
        then copy(state = State.Processing(instruction, remaining - 1), cycle = cycle + 1)
        else {
          val nextRegister = instruction match
            case Instruction.Noop => register
            case Instruction.Addx(i) => register + i
          val (nextState, remainingInstructions) = instructions match
            case next :: rest => State.Processing(next, next.duration) -> rest
            case Nil => State.Finished -> Nil
          copy(register = nextRegister, state = nextState, instructions = remainingInstructions, cycle = cycle + 1)
        }
      case State.Finished => this
  }

  object CPU {
    def init(instructions: List[Instruction]): CPU = instructions match
      case first :: rest => CPU(1, State.Processing(first, first.duration), rest, 1)
      case Nil => CPU(1, State.Finished, Nil, 1)
  }

  override def solve1Impl(input: Input): IO[Any] = {
    val interestingCycles = Set(20, 60, 100, 140, 180, 220)
    var cpu = CPU.init(input)
    val log = ListBuffer[(Int, Int)]()
    while (cpu.state != State.Finished)
      log.append(cpu.cycle -> cpu.register)
      cpu = cpu.next
    val out = log.toList

    IO.pure(out.collect {
      case (cycle, register) if interestingCycles.contains(cycle) => register * cycle
    }).map(_.sum)
  }

  override def solve2Impl(input: Input): IO[Any] = IO {
    var cpu = CPU.init(input)
    val log = ListBuffer[(Int, Int)]()
    while (cpu.state != State.Finished)
      log.append(cpu.cycle -> cpu.register)
      cpu = cpu.next
    val out = log.toList

    val res = StringBuilder()
    res.append("\n")
    out.grouped(40).foreach { x =>
      x.foreach { case (cycle, register) =>
        val col = (cycle % 40) - 1
        val lit = Set(register - 1, register, register + 1)
        res.append(if lit(col) then " # " else " . ")
      }
      res.append("\n")
    }
    res.toString()
  }
}
