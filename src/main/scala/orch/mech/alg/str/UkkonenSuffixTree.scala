package orch.mech.alg.str


class UkkonenSuffixTree private[str](val s: String, alphabet: Alphabet, val childrenCreator: () => NodeList) extends SuffixTree {
  private val UNKNOWN: Int = -1
  private val ROOT = newNode(UNKNOWN, -2)
  private var nodeCount = 0
  private val activePoint: ActivePoint = new ActivePoint(ROOT, -1, 0)
  private var leafEnd = UNKNOWN
  private var remainingSuffix = 0
  private var lastCreatedNode: NodeImpl = _
  private val text = alphabet.terminal().map(s + _).getOrElse(s)
  val string: String = text
  val strLen: Int = string.length

  // used for suffix array creation
  private var suffixArrayIdx = 0

  private class NodeImpl(val id: Int,
                         var start: Int,
                         var ending: Int = UNKNOWN,
                         val children: NodeList,
                         var suffixLink: NodeImpl,
                         var parent: NodeImpl) extends Node {
    def next(): NodeImpl = {
      parent.children.nextFrom(string.charAt(start)).asInstanceOf[NodeImpl]
    }

    def insert(c: Char, node: NodeImpl): Unit = {
      node.parent = this
      children(c) = node
    }
    def end: Int = if (ending == UNKNOWN) leafEnd else ending
  }
  private class ActivePoint(var node: NodeImpl, var edge: Int, var length: Int)
  override def root(): Node = ROOT

  build()

  private def newNode(start: Int, end: Int): NodeImpl = {
    var id = nodeCount
    nodeCount += 1
    new NodeImpl(id, start, end, childrenCreator(), null, null)
  }
  private def newNode(start: Int): NodeImpl = newNode(start, UNKNOWN)

  def build(): Unit = {
    var i = 0
    while (i < strLen) {
      extend(i)
      i += 1
    }
  }

  private def handleLoop(pos: Int): Boolean = {
    if (activePoint.length == 0) {
      activePoint.edge = pos
    }

    // check if edge exist
    val at = string.charAt(activePoint.edge)
    if (!activePoint.node.children.contains(at)) {
      newLeaf(at, pos)
    } else {
      val next = activePoint.node.children.get(at).asInstanceOf[NodeImpl]

      if (walkDown(next)) {
        // continue
        return false
      }

      // current character already on the edge
      if (string.charAt(next.start + activePoint.length) == string.charAt(pos)) {
        // If a newly created node waiting for it's
        // suffix link to be set, then set suffix link
        // of that waiting node to current active node
        if (lastCreatedNode != null && activePoint.node != null) {
          lastCreatedNode.suffixLink = activePoint.node
          lastCreatedNode = null
        }

        activePoint.length += 1

        // STOP all further processing in this phase and move on to next phase
        // break
        return true
      }

      val splitEnd = next.start + activePoint.length - 1
      val split = newNode(next.start, splitEnd)
      activePoint.node.insert(string.charAt(activePoint.edge), split)

      split.insert(string.charAt(pos), newNode(pos))
      next.start += activePoint.length
      split.insert(string.charAt(next.start), next)

      /** We got a new internal node here. If there is any
        * internal node created in last extensions of same
        * phase which is still waiting for it's suffix link
        * reset, do it now. */
      if (lastCreatedNode != null) {
        // suffixLink of lastNewNode points to current newly
        // created internal node
        lastCreatedNode.suffixLink = split
      }

      lastCreatedNode = split
    }
    // One suffix got added in tree, decrement the count of suffixes yet to be added.

    remainingSuffix -= 1
    if (activePoint.node == root && activePoint.length > 0) {
      activePoint.length -= 1
      activePoint.edge = pos - remainingSuffix + 1
    } else if (activePoint.node != root) {
      activePoint.node = activePoint.node.suffixLink
    }

    false
  }

  def extend(pos: Int): Unit = {
    // extend globally
    leafEnd = pos
    remainingSuffix += 1

    var break = false
    while (remainingSuffix > 0 && !break) {
      break = handleLoop(pos)
    }
  }


  private def walkDown(n: NodeImpl): Boolean = {
    val edgeLength = n.edgeLength
    if (activePoint.length >= edgeLength) {
      activePoint.edge += edgeLength
      activePoint.length -= edgeLength
      activePoint.node = n
      true
    } else false
  }

  private def newLeaf(at: Char, pos: Int): Unit = {
    //extension rule ann new leaf node
    activePoint.node.insert(at, newNode(pos))

    if (lastCreatedNode != null) {
      lastCreatedNode.suffixLink = activePoint.node
      lastCreatedNode = null
    }
  }

  def suffixArray(): SuffixArray = {
    val suffArray = Array.ofDim[Int](s.length)
    iterativeDFS(ROOT, suffArray, alphabet.terminal().isDefined)
    new SuffixArray(s, suffArray)
  }

  private def iterativeDFS(n: NodeImpl, arr: Array[Int], terminated: Boolean): Unit = {
    suffixArrayIdx = 0
    var curNode: NodeImpl = n
    var lastVisitedChild: NodeImpl = null
    var nextChild: NodeImpl = null
    var visit = true
    var ll = 0

    while (curNode != null) {
      if (visit) {
        ll += curNode.edgeLength

        if (curNode.children.isEmpty) {
          val idx = if (terminated) suffixArrayIdx - 1 else suffixArrayIdx
          if (idx >= 0) {
            arr(idx) = strLen - ll
          }
          suffixArrayIdx += 1
        }
      }

      if (lastVisitedChild == null) {
        if (curNode.children.isEmpty) {
          nextChild = null
        } else {
          nextChild = curNode.children.head().asInstanceOf[NodeImpl]
        }
      } else {
        nextChild = lastVisitedChild.next()
      }

      if (nextChild == null) {
        lastVisitedChild = curNode
        ll -= curNode.edgeLength
        curNode = curNode.parent
        visit = false
      } else {
        lastVisitedChild = null
        curNode = nextChild
        visit = true
      }
    }
  }

  def printTree(): Unit = {
    printNode(root(), 0)
  }

  private def printNode(n: Node, lvl: Int): Unit = {
    if (lvl == 0) {
      println(string)
    } else {
      (1 to lvl).foreach(i => print(" "))
      println(string.substring(n.start, n.end + 1) + s"[${n.start},${n.end}]")
    }

    n.children.foreach(nn => printNode(nn, lvl + 1))
  }

}