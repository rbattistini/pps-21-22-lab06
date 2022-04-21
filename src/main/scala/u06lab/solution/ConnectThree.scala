package u06lab.solution

import u06lab.solution.ConnectThree.{Player, computeAnyGame, computeAnyGameUntilWin, printBoards}

import java.util.OptionalInt
import scala.math.Ordering

object ConnectThree extends App:
  val bound = 3
  enum Player:
    case X, O
    def other: Player = this match
      case X => O
      case _ => X
  case class Disk(x: Int, y: Int, player: Player)
  /**
   * Board:
   * y
   *
   * 3
   * 2
   * 1
   * 0
   *   0 1 2 3 <-- x
   */
  type Board = Seq[Disk]
  type Game = Seq[Board]

  import Player.*

  def find(board: Board, x: Int, y: Int): Option[Player] =
    board find (d => d.x == x && d.y == y) map (_.player)

  def firstAvailableRow(board: Board, x: Int): Option[Int] =
    val z = board count(d => d.x == x); if z <= bound then Option(z) else Option.empty

  def placeAnyDisk(board: Board, player: Player): Seq[Board] =
    for
      x <- 0 to bound
      y = firstAvailableRow(board, x)
      if y.isDefined
    yield
      Disk(x, y.get, player) +: board

  def computeAnyGame(p: Player, moves: Int): LazyList[Game] = moves match
    case 0 => LazyList(Seq(Seq()))
    case _ => computeAnyGame(p.other, moves - 1) flatMap { g => placeAnyDisk(g.head, p) map (_ +: g)}

  def computeAnyGameUntilWin(p: Player, moves: Int): LazyList[Game] = moves match
    case 0 => LazyList(Seq(Seq()))
    case _ =>
      computeAnyGame(p.other, moves - 1)
        .flatMap(g => placeAnyDisk(g.head, p)
        .map(b => if !checkWin(b) then b +: g else g))

  def checkNeighbourhood(board: Board)(x1: Int, y1: Int)(x2: Int, y2: Int)(x3: Int, y3: Int): Boolean =
    (find(board, x1, y1), find(board, x2, y2), find(board, x3, y3)) match
    case (Some(p1), Some(p2), Some(p3)) if p1 == p2 && p1 == p3 => true
    case _ => false

  def checkDiagonalWin(board: Board): Boolean =
    (for
      i <- 1 until bound
    yield
      checkNeighbourhood(board)(i - 1, i - 1)(i, i)(i + 1, i + 1) ||
      checkNeighbourhood(board)(i + 1, i - 1)(i, i)(i - 1, i + 1) ||
      checkNeighbourhood(board)(i - 1, bound - (i - 1))(i, bound - i)(i + 1, bound - (i + 1))
    ).exists(identity)

  def checkHorizontalWin(b: Board): Boolean =
    (for
      y <- 0 to bound
      x <- 1 until bound
    yield
      checkNeighbourhood(b)(x - 1, y)(x, y)(x + 1, y)
    ).exists(identity)

  def checkVerticalWin(b: Board): Boolean =
    (for
      x <- 0 to bound
      y <- 1 until bound
    yield
      checkNeighbourhood(b)(x, y - 1)(x, y)(x, y + 1)
    ).exists(identity)

  def checkWin(b: Board): Boolean = checkHorizontalWin(b) && checkVerticalWin(b) && checkDiagonalWin(b)

  def printBoards(game: Seq[Board]): Unit =
    for
      y <- bound to 0 by -1
      board <- game.reverse
      x <- 0 to bound
    do
      print(find(board, x, y).map(_.toString).getOrElse("."))
      if x == bound then
        print(" ")
        if board == game.head then println()

@main def computeAnyGameTest() : Unit =
  import Player.*

  var states = 0
  computeAnyGame(O, 8).foreach { g =>
    printBoards(g)
    println()
    states += 1
  }
  print(states)

  states = 0
  computeAnyGameUntilWin(O, 8).foreach { g =>
    printBoards(g)
    println()
    states += 1
  }
  print(states)
