package chapter2

import chapter2.Chapter2._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Chapter2Test extends AnyFlatSpec with Matchers {
  "fib" should "return valid Fibonacci numbers" in {
    fib(0) should be(0)
    fib(1) should be(1)
    fib(3) should be(2)
    fib(6) should be(8)
  }

  "isSorted" should "return valid booleans" in {
    isSorted(Array(1, 2, 3), (x: Int, y: Int) => x < y) should be(true)
    isSorted(Array(5, 2, 0), (x: Int, y: Int) => x > y) should be(true)
    isSorted(Array(1, 5, 2), (x: Int, y: Int) => x < y) should be(false)
    isSorted(Array("Abc", "zeF", "Ghi"), (x: String, y: String) => x.toLowerCase.last < y.toLowerCase.last) should be(true)
  }

  "curry" should "curry function" in {
    def foo(a: Int, b: Boolean) = s"$a$b"
    val curriedFoo = curry(foo)
    foo(5, b = true) should be(curriedFoo(5)(true))
  }

  "uncurry" should "uncurry function" in {
    def foo(a: Int)(b: Boolean) = s"$a$b"
    val uncurriedFoo = uncurry(foo)
    foo(5)(b = true) should be(uncurriedFoo(5, true))
  }

  "compose" should "compose functions" in {
    def foo(b: Boolean) = if(b) "Greater than 5" else "Less or equal 5"
    def bar(a: Int) = a > 5
    val composed = compose(foo, bar)
    foo(bar(6)) should be(composed(6))
  }
}
