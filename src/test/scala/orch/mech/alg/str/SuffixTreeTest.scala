package orch.mech.alg.str


object SuffixTreeTest {
  def main(args: Array[String]): Unit = {
    val suffixTree = SuffixTree.ukkonen("aabaa", LowercaseTermination)
    // suffixTree.asInstanceOf[UkkonenSuffixTree].printTree()

    val sa = suffixTree.suffixArray()
    println(sa)

    // sa.print()

    val lcp = sa.lcp()
    println(lcp)
    println(distincCount(lcp))

  }

  def distincCount(lcp: LCPArray): Long = {
    val n = lcp.string.length
    (n * (n + 1) / 2) - sum(lcp.arr)
  }

  def sum(array: Array[Int]): Long = {
    var s = 0L

    var i = 0
    while (i < array.length) {
      s += array(i)
      i += 1
    }

    s
  }
}
