package orch.mech.alg.str


class SuffixArray(val string: String, val arr: Array[Int]) {
  def apply(i: Int): Int = arr(i)

  override def toString: String = {
    val limit = 10
    val overLimit = string.length < limit
    s"SA[S='${
      if (overLimit) string else string.take(limit) + "..."
    }', n=${string.length}, array=[${
      if (overLimit) arr.mkString(" ") else arr.take(limit).mkString("", " ", "...")
    }]]"
  }

  def print():Unit = {
    arr.foreach(i => println(s"$i ${string.substring(i)}"))
  }

  def inverse(): Array[Int] = {
    val invSuffix: Array[Int] = Array.ofDim(arr.length)
    var i = 0
    while (i < arr.length) {
      invSuffix(arr(i)) = i
      i += 1
    }
    invSuffix
  }

  def lcp(): LCPArray = new LCPArray(string, buildKasai())

  private def buildKasai(): Array[Int] = {
    val strLen: Int = string.length
    val invSuffix = inverse()
    val lcpArr: Array[Int] = Array.fill[Int](strLen - 1)(0)

    var k = 0

    var i = 0
    while (i < strLen) {
      if (invSuffix(i) == strLen - 1) {
        k = 0
        // continue
      } else {
        var j = 0

        j = arr(invSuffix(i) + 1)
        while (i + k < strLen &&
          j + k < strLen &&
          string.charAt(i + k) == string.charAt(j + k)) {
          k += 1
        }

        lcpArr(invSuffix(i)) = k

        if (k > 0) {
          k -= 1
        }
      }

      i += 1
    }

    lcpArr
  }

}

