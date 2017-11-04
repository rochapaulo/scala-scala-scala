package huffman

/**
  * HuffmanCoding
  *
  * In computer science and information theory, a Huffman code is a particular type of optimal prefix code
  * that is commonly used for lossless data compression. The process of finding and/or using such a code
  * proceeds by means of Huffman coding, an algorithm developed by David A.
  * Huffman while he was a Sc.D. student at MIT,
  * and published in the 1952 paper "A Method for the Construction of Minimum-Redundancy Codes
  *
  * @author rochapaulo
  * @since 02/07/2017
  *
  */
object HuffmanCoding {

  type Bit = Int
  type CodeTable = List[(Char, List[Bit])]

  /**
    * Decode tree using given sequence of bits
    *
    * @param root The tree root
    * @param bits The sequence of bits to decode
    * @return the sequence decoded
    *
    */
  def decode(root: CodeTree, bits: List[Bit]): List[Char] = {

    def decode(bits: List[Bit], fork: CodeTree, output: List[Char]): List[Char] = {
      fork match {
        case Leaf(char, _) => {
          if (bits.isEmpty) {
            output :+ char
          } else {
            decode(bits, root, output :+ char)
          }
        }
        case Fork(left, right, _, _) => bits.head match {
          case 0 => decode(bits.tail, left, output)
          case 1 => decode(bits.tail, right, output)
        }
      }
    }

    decode(bits, root, List())
  }

  /**
    * Encode given text in a sequence of bits
    *
    * @param root The tree root
    * @param text The text to be encoded
    * @return The encoded sequence of bits
    *
    */
  def encode(root: CodeTree)(text: List[Char]): List[Bit] = {

    def encode(fork: CodeTree, expected: Char, output: List[Bit] = List()): List[Bit] = {
      fork match {
        case Leaf(char, _) => if (char.equals(expected)) output else List()
        case Fork(left, right, _, _) => {
          encode(left, expected, output :+ 0) ::: encode(right, expected, output :+ 1)
        }
      }
    }

    text.flatMap(encode(root, _))
  }

  /**
    * Encode given text in a sequence of bits
    *
    * @param root The tree root
    * @param text The text to be encoded
    * @return The encoded sequence of bits
    *
    */
  def quickEncode(root: CodeTree)(text: List[Char]): List[Bit] = {
    val table = convert(root)
    text.flatMap(codeBits(table)) _
  }

  private[huffman] def convert(root: CodeTree): CodeTable = {

    def convert(fork: CodeTree, bits: List[Bit]): CodeTable = {
      fork match {
        case Leaf(char, _) => List[(Char, List[Int])]((char, bits))
        case Fork(left, right, _, _) => mergeCodeTables(convert(left, bits :+ 0), convert(right, bits :+ 1))
      }
    }
    convert(root, List())
  }

  private[huffman] def mergeCodeTables(a: CodeTable, b: CodeTable) : CodeTable = a ::: b

  private[huffman] def codeBits(table: CodeTable)(char: Char): List[Bit] = {
    table.filter(e => char.equals(e._1)).flatMap(e => e._2)
  }

}