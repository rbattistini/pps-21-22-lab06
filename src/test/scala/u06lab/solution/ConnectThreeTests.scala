package u06lab.solution

import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit.Test
import u06lab.solution.ConnectThree.*
import u06lab.solution.ConnectThree.Player.*

class ConnectThreeTests:
  
  @Test
  def findTestX(): Unit =
    assertEquals(Some(X), find(Seq(Disk(0, 1, X)), 0, 1))

  @Test
  def findTestO(): Unit =
    assertEquals(Some(O), find(Seq(Disk(1, 0, X), Disk(2, 1, X), Disk(1, 2, O)), 1, 2))

  @Test
  def findEmpty(): Unit =
    assertEquals(None, find(Seq(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 1, 1))

  @Test
  def firstAvailableRowTestFirstRow(): Unit =
    assertEquals(Some(0), firstAvailableRow(Seq(), 0))

  @Test
  def firstAvailableRowTestLastRow(): Unit =
    assertEquals(Some(3), firstAvailableRow(Seq(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 0))

  @Test
  def firstAvailableRowTestNone(): Unit =
    assertEquals(None, firstAvailableRow(Seq(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X), Disk(0, 3, X)), 0))

  @Test
  def placeAnyDiskTestEmptyBoard(): Unit =
    assertEquals(
      Seq(
        Seq(Disk(0, 0, X)),
        Seq(Disk(1, 0, X)),
        Seq(Disk(2, 0, X)),
        Seq(Disk(3, 0, X))
      ),
      placeAnyDisk(Seq(), X)
    )

  @Test
  def placeAnyDiskTestNonEmptyBoard(): Unit =
    assertEquals(
      Seq(
        Seq(Disk(0, 1, X), Disk(0, 0, O)),
        Seq(Disk(1, 0, X), Disk(0, 0, O)),
        Seq(Disk(2, 0, X), Disk(0, 0, O)),
        Seq(Disk(3, 0, X), Disk(0, 0, O))
      ),
      placeAnyDisk(Seq(Disk(0, 0, O)), X)
    )
    