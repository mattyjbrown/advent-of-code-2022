package dev.matthewbrown.days

import cats.effect.IO

import scala.collection.mutable

object Day12 extends BasicDay {
  override val day: Int = 12
  override type Input = ParseResult

  case class Cell(x: Int, y: Int) {
    override def toString: String = s"($x,$y)"
    def neighbours: Set[Cell] = Set(
      Cell(x, y - 1),
      Cell(x, y + 1),
      Cell(x - 1, y),
      Cell(x + 1, y),
    )
  }
  case class ParseResult(grid: Vector[Vector[Char]], start: Cell, end: Cell) {
    def at(cell: Cell): Char = grid(cell.x)(cell.y)
  }
  override def parse(strs: Vector[String]): IO[Input] = IO {
    val grid = strs.transpose
    var startX: Int = 999999
    var startY: Int = 999999
    var endX: Int = 999999
    var endY: Int = 999999
    for {
      x <- grid.zipWithIndex
      y <- x._1.zipWithIndex
    } yield {
      if y._1 == 'S'
      then
        startX = x._2
        startY = y._2

      if y._1 == 'E'
      then
        endX = x._2
        endY = y._2
    }
    ParseResult(grid.map(_.map(c => if c == 'S' then 'a' else c).map(c => if c == 'E' then 'z' else c)), Cell(startX, startY), Cell(endX, endY))
  }

//  override def solve1Impl(input: Input): IO[Any] = IO {
//    //var calls: Int = 0
//    //Need a recursive method that takes:
//    //A set of cells previously visited - visited: Set[Cell]
//    //The current cell - current: Cell
//    //The path so far - path: List[Cell]
//    //Returns the smallest path - List[Cell]
//    //Actually Option[List[Cell]] - none for dead-end/no-path
//    //For each neighbour, filtered to the possible and unvisited ones, recurse with current cell on path and visited, return smallest
//
//    //OK that was garbage, waaaay too many duplicates.
//    //Maintain a set of shortest paths to ALL cells - so if you get to a cell, and you aren't the first one there, don't use it.
//    //Has to be outside the recursive method because it needs knowledge of other branches
//
//    //I had to give it -Xss1G to make it not stack overflow, but it did work...
//    val shortestPaths: mutable.Map[Cell, Int] = mutable.HashMap()
//    def rec(current: Cell, path: List[Cell], visited: Set[Cell]): Option[List[Cell]] = {
//      //calls = calls + 1
//      //println(s"$calls: $current")
//      if current == input.end
//      then Some(current :: path)
//      else if shortestPaths.get(current).exists(_ <= path.size) then None //Someone else has a shorter or equivalent path to me, so no point carrying on
//      else
//        shortestPaths.update(current, path.size)
//        val value = input.at(current)
//        val valids = current.neighbours
//          .filter(c => c.x >= 0 && c.y >= 0 && c.x < input.grid.size && c.y < input.grid.head.size)
//          .filterNot(visited)
//          .filter(c => input.at(c) - value <= 1)
//        val pathsFromHere = valids.flatMap(cell => rec(cell, current :: path, visited + current))
//        pathsFromHere.minByOption(_.length)
//    }
//    val res = rec(input.start, Nil, Set.empty)
//    println(res.mkString("->"))
//    res.get.size - 1
//  }

  /**
   * Let's try a different approach.
   * Start at the end.
   * Each step, get the valids, measure
   */
  override def solve2Impl(input: Input): IO[Any] = IO {
    var steps: Int = 0
    var visited: Set[Cell] = Set(input.end)
    var gotTo: Set[Cell] = Set(input.end)
    var done: Boolean = false
    while (!done)
      steps = steps + 1
      val nextPossibilities: Set[Cell] = gotTo.flatMap(current => current.neighbours
        .filter(neighbour => neighbour.x >= 0 && neighbour.y >= 0 && neighbour.x < input.grid.size && neighbour.y < input.grid.head.size)
        .filterNot(visited)
        .filter(neighbour => input.at(current) - input.at(neighbour) <= 1)
        )
      if nextPossibilities.exists(c => input.at(c) == 'a')
      then done = true
      else
        visited = visited ++ gotTo
        gotTo = nextPossibilities

    steps
  }

  /**
   * Let's re-do pt1 in the style of pt2
   */
  override def solve1Impl(input: ParseResult): IO[Any] = IO {
    var steps: Int = 0
    var visited: Set[Cell] = Set.empty
    var gotTo: Set[Cell] = Set(input.start)
    var done: Boolean = false
    while (!done)
      steps = steps + 1
      val nextPossibilities: Set[Cell] = gotTo.flatMap(current => current.neighbours
        .filter(neighbour => neighbour.x >= 0 && neighbour.y >= 0 && neighbour.x < input.grid.size && neighbour.y < input.grid.head.size)
        .filterNot(visited)
        .filter(neighbour => input.at(neighbour) - input.at(current)<= 1)
      )
      if nextPossibilities.contains(input.end)
      then done = true
      else
        visited = visited ++ gotTo
        gotTo = nextPossibilities

    steps
  }

}
