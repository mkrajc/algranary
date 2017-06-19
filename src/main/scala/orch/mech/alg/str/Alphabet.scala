package orch.mech.alg.str


trait Alphabet {
  def contains(c: Char): Boolean
  def size: Int
  // must support terminal char indexing if present and index it to 0
  def index(c: Char): Int
  // character that is not part of alphabet and will be used as termination if present
  def terminal(): Option[Char]
}

case object LowercaseTermination extends Alphabet {
  private val ab = 'a' to 'z'
  override def contains(c: Char): Boolean = ab.contains(c)
  override val size: Int = 1 + ab.size
  override def index(c: Char): Int = math.max(0, 1 + c - 'a')
  override def terminal(): Option[Char] = Some('$')
}

case object LowercaseStandard extends Alphabet {
  private val ab = 'a' to 'z'
  override def contains(c: Char): Boolean = ab.contains(c)
  override val size: Int = ab.size
  override def index(c: Char): Int =  c - 'a'
  override def terminal(): Option[Char] = None
}

