package huffman

/**
  * CodeTree
  *
  * @author rochapaulo
  * @since 02/07/2017
  *
  */
object CodeTree {

  def create(chars: List[Char]): CodeTree = {
    until(singleton, combine)(makeOrderedLeafList(times(chars)))
  }

  private[huffman] def weight(tree: CodeTree): Int = tree match {
    case Fork(_, _, _, weight) => weight
    case Leaf(_, weight) => weight
  }

  private[huffman] def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(_, _, chars, _) => chars
    case Leaf(chars, _) => List(chars)
  }

  private[huffman] def until(
                              singleton: (List[CodeTree]) => Boolean,
                              combine: (List[CodeTree]) => List[CodeTree])(tree: List[CodeTree]): CodeTree = {

    if (singleton(tree)) tree.head else until(singleton, combine)(combine(tree))
  }

  private[huffman] def singleton(trees: List[CodeTree]): Boolean = {
    trees.size == 1
  }

  private[huffman] def combine(trees: List[CodeTree]): List[CodeTree] = {
    trees.grouped(2).map(x => if (singleton(x)) x.head else merge(x(0), x(1))).toList
  }

  private[huffman] def merge(left: CodeTree, right: CodeTree): CodeTree = {
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))
  }

  private[huffman] def makeOrderedLeafList(frequencies: List[(Char, Int)]): List[Leaf] = {
    def weightComparator(a: (Char, Int), b: (Char, Int)) = a._2 <= b._2
    frequencies.sortWith(weightComparator).map(pair => Leaf(pair._1, pair._2))
  }

  private[huffman] def times(sequence: List[Char]): List[(Char, Int)] = {
    sequence.groupBy(identity).map(v => (v._1, v._2.length)).toList
  }

}
abstract class CodeTree
case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
case class Leaf(char: Char, weight: Int) extends CodeTree