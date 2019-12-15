import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import org.scalatest.Matchers
import org.scalatest.FlatSpec

import Element.elem

class ElementSuite extends FunSuite with BeforeAndAfter {
  before {
    println("before")
  }

  after {
    println("after")
  }

  test("elem result should have passed width") {

    val ele = elem('x', 2, 3)
    assert(ele.width == 2)
    assertResult(2) {
      ele.width
    }
  }

  // test("Throws exception") {
  //   assertThrows[IllegalArgumentException] {
  //     elem('x', -2, 3)
  //   }
  // }
}

class ElementSpec2 extends FlatSpec with Matchers {
  "A UniformElement" should
    "have a width equal to the passed value" in {
    val ele = elem('x', 2, 3)
    ele.width should be(2)
  }
  it should "have a height equal to the passed value" in {
    val ele = elem('x', 2, 3)
    ele.height should be(3)
  }
  it should "throw an IAE if passed a negative width" in {
    an[IllegalArgumentException] should be thrownBy {
      elem('x', -2, -3)
    }
  }
}
