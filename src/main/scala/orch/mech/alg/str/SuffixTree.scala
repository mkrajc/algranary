package orch.mech.alg.str


trait SuffixTree {
  val string: String
  def root(): Node
  def suffixArray(): SuffixArray
}

object SuffixTree {
  def ukkonen(s: String, alphabet: Alphabet): SuffixTree = {
    val creator = () => new ArrayNodeList(alphabet)
    new UkkonenSuffixTree(s, alphabet, creator)
  }
}

trait Node {
  def children: NodeList
  def id: Int
  def edgeLength: Int = end - start + 1
  def start: Int
  def end: Int
}

trait NodeList {
  def head(): Node
  def nextFrom(c: Char): Node
  def get(at: Char): Node
  def update(at: Char, node: Node): Unit
  def contains(c: Char): Boolean
  def foreach(f: (Node => Unit))
  def isEmpty: Boolean
}

class ArrayNodeList(a: Alphabet) extends NodeList {
  private val arr: Array[Node] = Array.ofDim[Node](a.size)
  private var sz = 0

  override def get(c: Char): Node = arr(a.index(c))
  override def update(c: Char, node: Node): Unit = {
    sz += 1
    arr(a.index(c)) = node
  }
  override def contains(c: Char): Boolean = arr(a.index(c)) != null
  override def isEmpty: Boolean = sz == 0
  override def foreach(f: (Node) => Unit): Unit = {
    var i = 0
    while (i < a.size) {
      if (arr(i) != null) {
        f(arr(i))
      }
      i += 1
    }
  }
  override def head(): Node = fromIndex(0)
  override def nextFrom(c: Char): Node = fromIndex(a.index(c) + 1)
  private def fromIndex(idx: Int): Node = {
    var i = idx
    while (i < a.size) {
      if (arr(i) != null) {
        return arr(i)
      }
      i += 1
    }
    null
  }
}