package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("times") {
    assert(times(List('a', 'b', 'a')) === List(('a', 2), ('b', 1)))
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))


    val anotherList = List(Leaf('a', 3), Leaf('b', 3), Leaf('c', 4))
    assert(combine(anotherList) === List(Leaf('c',4), Fork(Leaf('a', 3), Leaf('b',3), List[Char]('a','b'), 6)))
  }

  test("create code tree"){
    assert(createCodeTree("helloworld".toCharArray.toList) === Fork(Fork(Fork(Leaf('w',1),Leaf('r',1),List('w', 'r'),2),Leaf('o',2),List('w', 'r', 'o'),4),Fork(Fork(Leaf('d',1),Fork(Leaf('h',1),Leaf('e',1),List('h', 'e'),2),List('d', 'h', 'e'),3),Leaf('l',3),List('d', 'h', 'e', 'l'),6),List('w', 'r', 'o', 'd', 'h', 'e', 'l'),10))
  }

  test("test encode french") {
    assert(decodedSecret === List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l'))
  }

  test("convert to CodeTable"){
    assert(convert(createCodeTree("helloworld".toCharArray.toList)) === List(('w',List(0, 0, 0)), ('r',List(0, 0, 1)), ('o',List(0, 1)), ('d',List(1, 0, 0)), ('h',List(1, 0, 1, 0)), ('e',List(1, 0, 1, 1)), ('l',List(1, 1))))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("ultimate test"){
    val tree = createCodeTree("The quick brown fox jumps over the lazy dog".toCharArray.toList)
    println(quickEncode(tree)("is this right?".toCharArray.toList))
  }
}
