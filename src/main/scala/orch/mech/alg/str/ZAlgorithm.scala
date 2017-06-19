package orch.mech.alg.str


class ZAlgorithm {

  def zAlgorithm(s: String): Array[Int] = {
    val len = s.length
    val Z = Array.ofDim[Int](len)

    var L = 0
    var R = 0
    var k = 0

    var i = 1
    while (i < len) {

      if (i > R) {
        L = i
        R = i

        while (R < len && s.charAt(R - L) == s.charAt(R)) {
          R += 1
        }

        Z(i) = R - L
        R -= 1
      } else {
        k = i - L
        if (Z(k) < R - i + 1) {
          Z(i) = Z(k)
        } else {
          L = i

          while (R < len && s.charAt(R - L) == s.charAt(R)) {
            R += 1
          }

          Z(i) = R - L
          R -= 1
        }
      }

      i += 1
    }

    Z
  }
}

