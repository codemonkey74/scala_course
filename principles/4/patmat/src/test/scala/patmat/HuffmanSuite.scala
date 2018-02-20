package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val a1 = List(0)
    val b1 = List(1)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val a2 = List(0, 0)
    val b2 = List(0, 1)
    val d2 = List(1)
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


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("createCodeTree creates a CodeTree") {
    assert(createCodeTree("aaabbc".toList) === Fork(Fork(Leaf('c',1),Leaf('b',2),List('c', 'b'),3),Leaf('a',3),List('c', 'b', 'a'),6))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode should find text a") {
    new TestTrees {
      assert(decode(t1, a1) === "a".toList)
    }
  }

  test("decode should find text b") {
    new TestTrees {
      assert(decode(t1, b1) === "b".toList)
    }
  }

  test("decode should find text ab") {
    new TestTrees {
      assert(decode(t1, a1 ++ b1) === "ab".toList)
    }
  }

  test("decode should find text abaab") {
    new TestTrees {
      assert(decode(t1, a1 ++ b1 ++ a1 ++ a1 ++ b1) === "abaab".toList)
    }
  }

  test("decode should find text abd") {
    new TestTrees {
      assert(decode(t2, a2 ++ b2 ++ d2) === "abd".toList)
    }
  }

  test ("decodeSecret should find something") {
    assert(decodedSecret === "HuffmanEstCool".toLowerCase.toList)
  }

  test ("encode should encode secret") {
    assert(encode(frenchCode)("HuffmanEstCool".toLowerCase.toList) === secret)
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("quickEncode should encode secret") {
    assert(quickEncode(frenchCode)("HuffmanEstCool".toLowerCase.toList) === secret)
  }

}
