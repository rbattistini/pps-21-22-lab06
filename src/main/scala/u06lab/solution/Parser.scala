package u06lab.solution

/** Consider the Parser example shown in previous lesson. Analogously to NonEmpty, create a mixin NotTwoConsecutive,
  * which adds the idea that one cannot parse two consecutive elements which are equal. Use it (as a mixin) to build
  * class NotTwoConsecutiveParser, used in the testing code at the end. Note we also test that the two mixins can work
  * together!!
  */

abstract class Parser[T]:
  def parse(t: T): Boolean // is the token accepted?
  def end: Boolean // is it ok to end here
  def parseAll(seq: Seq[T]): Boolean = (seq forall parse) & end // note &, not &&

object Parsers:
  extension (s: String)
     def charParser(): Parser[Char] = BasicParser(s.toSet)

class BasicParser(chars: Set[Char]) extends Parser[Char]:
  override def parse(t: Char): Boolean = chars.contains(t)
  override def end: Boolean = true

trait NonEmpty[T] extends Parser[T]:
  private[this] var empty = true
  abstract override def parse(t: T): Boolean =
    empty = false
    super.parse(t) // who is super??
  abstract override def end: Boolean = !empty && super.end

class NonEmptyParser(chars: Set[Char]) extends BasicParser(chars) with NonEmpty[Char]

trait NotTwoConsecutive[T] extends Parser[T]:
  private[this] var previous: T = _
  abstract override def parse(t: T): Boolean =
    if previous == t then false else {previous = t; super.parse(t)}
  abstract override def end: Boolean = super.end

class NotTwoConsecutiveParser(chars: Set[Char]) extends BasicParser(chars) with NotTwoConsecutive[Char]
