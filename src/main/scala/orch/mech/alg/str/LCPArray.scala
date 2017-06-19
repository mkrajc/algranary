package orch.mech.alg.str

class LCPArray(val string: String, val arr: Array[Int]) {
  override def toString: String = {
    val limit = 10
    val overLimit = string.length < limit
    s"LCP[S='${
      if (overLimit) string else string.take(limit) + "..."
    }', n=${string.length}, array=[${
      if (overLimit) arr.mkString(" ") else arr.take(limit).mkString("", " ", "...")
    }]]"
  }
}

