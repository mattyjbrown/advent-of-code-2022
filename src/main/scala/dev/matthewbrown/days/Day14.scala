package dev.matthewbrown.days

import cats.effect.IO
import dev.matthewbrown.Utils.IntExtractor
import dev.matthewbrown.days.Day12.Cell

object Day14 extends BasicDay {
  override val day: Int = 14

  enum Stuff:
    case Rock
    case Sand
    case Air

    final def toChar: Char = this match
      case Stuff.Rock => '#'
      case Stuff.Sand => 'o'
      case Stuff.Air => '.'

  case class Cell(x: Int, y: Int) {
    override def toString: String = s"($x,$y)"
    def downOne: Cell = copy(y = y + 1)
    def downLeft: Cell = copy(x = x - 1, y = y + 1)
    def downRight: Cell = copy(x = x + 1, y = y + 1)
  }

  object Cell:
    def parse(str: String): Cell = str.split(',') match
      case Array(IntExtractor(x), IntExtractor(y)) => Cell(x, y)
      case _ => throw InvalidInput("Not a cell", str)

  case class Grid(grid: Array[Array[Stuff]]):
    def at(cell: Cell): Stuff = grid(cell.x)(cell.y)
    def maxY: Int = grid.head.length - 1
    def maxX: Int = grid.length - 1
    def atWithFloor(cell: Cell): Stuff =
      try
        grid(cell.x)(cell.y)
      catch
        case e: ArrayIndexOutOfBoundsException =>
          if cell.y == maxY + 1 then Stuff.Rock
          else if cell.x == maxX + 1 then Stuff.Air
          else if cell.x == -1 then Stuff.Air
          else throw e

  override type Input = Grid


  /**
   * Each row describes a line, vertical or horizontal
   */
  override def parse(strs: Vector[String]): IO[Input] = IO {
    val rockCells = strs.map(_.split(" -> ".toArray).filter(_.nonEmpty).map(Cell.parse)).flatMap(_.foldLeft(List.empty[Cell]){(acc, next) =>
      acc match
        case head :: tail =>
          //Add all the cells in between head and me, including me
          val cells: List[Cell] =
            if head.x == next.x && head.y < next.y
            then (head.y to next.y).map(y => Cell(head.x, y)).toList.reverse
            else if head.x == next.x && head.y > next.y
            then (next.y to head.y).map(y => Cell(head.x, y)).toList
            else if head.y == next.y && head.x < next.x
            then (head.x to next.x).map(x => Cell(x, head.y)).toList.reverse
            else if head.y == next.y && head.x > next.x
            then (next.x to head.x).map(x => Cell(x, head.y)).toList
            else throw InvalidInput(s"Two cells that aren't horizontal or vertical", s"$next and $head")
          cells ::: tail
        case Nil =>
          //I am the first cell in my row
          List(next)
    })
    val maxX = rockCells.maxBy(_.x).x + 1
    val maxY = rockCells.maxBy(_.y).y + 1
    val resGrid: Array[Array[Stuff]] = Array.fill(maxX, maxY)(Stuff.Air)
    rockCells.foreach(cell => resGrid(cell.x)(cell.y) = Stuff.Rock)
    Grid(resGrid)
  }

  def printGrid(grid: Grid): Unit =
    val firstX = grid.grid.transpose.map(_.indexWhere(_ != Stuff.Air)).filter(_ > 0).min
    grid.grid.transpose.foreach(row => println(row.drop(firstX - 1).map(_.toChar).mkString))

  override def solve1Impl(input: Input): IO[Any] = IO {
    def dropSand(grid: Grid, startingAt: Cell): Grid = {
      def rec(sandAt: Cell): Cell =
        if grid.at(sandAt.downOne) == Stuff.Air then rec(sandAt.downOne)
        else if grid.at(sandAt.downLeft) == Stuff.Air then rec(sandAt.downLeft)
        else if grid.at(sandAt.downRight) == Stuff.Air then rec(sandAt.downRight)
        else sandAt

      val sandFinishesAt = rec(startingAt)
      val newGrid = grid.grid.clone()
      newGrid(sandFinishesAt.x)(sandFinishesAt.y) = Stuff.Sand
      Grid(newGrid)
    }

    var grid = input
    var dropped = 0

    println("Started as:")
    printGrid(input)

    try
      while(true)
        grid = dropSand(grid, Cell(500, 0))
        dropped = dropped + 1
    catch
      case _: ArrayIndexOutOfBoundsException => println(s"Sand fell out, finishing at $dropped")

    println("Finished as:")
    printGrid(grid)

    dropped
  }

  override def solve2Impl(input: Input): IO[Any] = IO {
    def dropSandWithFloor(grid: Grid, startingAt: Cell): Grid = {
      def rec(sandAt: Cell): Cell =
        if grid.atWithFloor(sandAt.downOne) == Stuff.Air then rec(sandAt.downOne)
        else if grid.atWithFloor(sandAt.downLeft) == Stuff.Air then rec(sandAt.downLeft)
        else if grid.atWithFloor(sandAt.downRight) == Stuff.Air then rec(sandAt.downRight)
        else sandAt

      val sandFinishesAt = rec(startingAt)
      val newGrid = grid.grid.clone()
      newGrid(sandFinishesAt.x)(sandFinishesAt.y) = Stuff.Sand
      Grid(newGrid)
    }

    val extraWidth = 140
    var grid = Grid(input.grid
      .map(_.appended(Stuff.Air)) //Add an air to the bottom of every column
      .map(_.appended(Stuff.Rock)) //Add a rock to the bottom of every column
      .appendedAll(Array.fill(extraWidth)(Array.fill(input.maxY + 2)(Stuff.Air).appended(Stuff.Rock)))//Insert 10000 columns at the start which are all air but end in rock
      .prependedAll(Array.fill(extraWidth)(Array.fill(input.maxY + 2)(Stuff.Air).appended(Stuff.Rock)))//Insert 10000 columns at the end which are all air but end in rock
    )
    var dropped = 0
    var finished = false
    println("Started as:")
    printGrid(grid)

    while (!finished)
      println(dropped)
      grid = dropSandWithFloor(grid, Cell(500 + extraWidth, 0))
      dropped = dropped + 1
      if grid.at(Cell(500 + extraWidth, 0)) == Stuff.Sand then finished = true

    println("Finished as:")
    printGrid(grid)

    dropped
  }
}
