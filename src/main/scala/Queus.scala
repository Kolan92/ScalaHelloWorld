import scala.collection.mutable.ArrayBuffer

abstract class IntQueue {
  def get(): Option[Int]
  def put(x: Int)
}

class BasicIntQueue extends IntQueue {
  private val buf = new ArrayBuffer[Int]
  def get(): Option[Int] = {
    if (buf.length > 0)
      Option(buf.remove(0))
    else
      Option.empty
  }
  def put(x: Int) = { buf += x }
}

trait Doubling extends IntQueue {
  abstract override def put(x: Int) = { super.put(2 * x) }
}

trait Filtering extends IntQueue {
  abstract override def put(x: Int) = {
    if (x >= 100)
      super.put(x)
  }
}
