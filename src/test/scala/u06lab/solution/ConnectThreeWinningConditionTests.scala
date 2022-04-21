package u06lab.solution

import org.junit.Assert.{assertFalse, assertTrue}
import org.junit.Test
import u06lab.solution.ConnectThree.Player.{O, X}
import u06lab.solution.ConnectThree.{Disk, checkDiagonalWin, checkHorizontalWin, checkVerticalWin}

class ConnectThreeWinningConditionTests {

  @Test
  def mainDiagonalWinTest() : Unit =
    assertTrue(checkDiagonalWin(Seq(Disk(3, 1, X), Disk(2, 2, X), Disk(1, 3, X))))

  @Test
  def mainDiagonalNotWinTest() : Unit =
    assertFalse(checkDiagonalWin(Seq(Disk(3, 0, X), Disk(2, 1, X), Disk(1, 2, O), Disk(0, 3, X))))

  @Test
  def mainDiagonalOutOfBoundTest() : Unit =
    assertFalse(checkDiagonalWin(Seq(Disk(4, 0, X), Disk(3, 1, X), Disk(2, 2, X))))


  @Test
  def counterDiagonalWinTest(): Unit =
    assertTrue(checkDiagonalWin(Seq(Disk(1, 1, X), Disk(2, 2, X), Disk(3, 3, X))))

  @Test
  def counterDiagonalNotWinTest(): Unit =
    assertFalse(checkDiagonalWin(Seq(Disk(0, 0, X), Disk(1, 1, O), Disk(2, 2, X), Disk(3, 3, X))))

  @Test
  def counterDiagonalOutOfUpperBoundTest(): Unit =
    assertFalse(checkDiagonalWin(Seq(Disk(2, 2, X), Disk(3, 3, X), Disk(4, 4, X))))

  @Test
  def counterDiagonalOutOfLowerBoundTest(): Unit =
    assertFalse(checkDiagonalWin(Seq(Disk(-1, -1, X), Disk(0, 0, X), Disk(1, 1, X))))


  @Test
  def horizontalWinTest() : Unit =
    assertTrue(checkHorizontalWin(Seq(Disk(1, 0, X), Disk(2, 0, X), Disk(3, 0, X))))

  @Test
  def horizontalNotWinTest() : Unit =
    assertFalse(checkHorizontalWin(Seq(Disk(0, 0, X), Disk(1, 0, X), Disk(2, 0, O), Disk(3, 0, X))))

  @Test
  def horizontalOutOfUpperBoundTest() : Unit =
    assertFalse(checkHorizontalWin(Seq(Disk(2, 0, X), Disk(3, 0, X), Disk(4, 0, X))))

  @Test
  def horizontalOutOfLowerBoundTest() : Unit =
    assertFalse(checkHorizontalWin(Seq(Disk(-1, 0, X), Disk(0, 0, X), Disk(1, 0, X))))


  @Test
  def verticalWinTest() : Unit =
    assertTrue(checkVerticalWin(Seq(Disk(0, 1, X), Disk(0, 2, X), Disk(0, 3, X))))

  @Test
  def verticalNotWinTest() : Unit =
    assertFalse(checkVerticalWin(Seq(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X), Disk(0, 3, X))))

  @Test
  def verticalOutOfUpperBoundTest() : Unit =
    assertFalse(checkVerticalWin(Seq(Disk(0, 2, X), Disk(0, 3, X), Disk(0, 4, X))))

  @Test
  def verticalOutOfLowerBoundTest() : Unit =
    assertFalse(checkVerticalWin(Seq(Disk(0, -1, X), Disk(0, 0, X), Disk(0, 1, X))))
}
