package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.8/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val s5 = singletonSet(5)
    val s7 = singletonSet(7)
    val s1000 = singletonSet(1000)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }


  test("intersect has elements in both sets") {
    new TestSets {
      var c1 = union(s1,s2)
      var c2 = union(s2,s3)
      val inter = intersect(c1,c2)
      assert(contains(inter, 2), "Intercept 1")
      assert(!contains(inter, 1), "Intercept 2")
      assert(!contains(inter, 3), "Intercept 3")
    }
  }

  test("diff has elements that are not in both sets") {
    new TestSets {
      var c1 = union(s1,s2)
      var diffs = diff(c1, s3)
      assert(contains(diffs, 1), "Diff 1")
      assert(contains(diffs, 2), "Diff 2")
      assert(!contains(diffs, 3), "Diff 3")
    }
  }

  test("filter has elements that pass a given condition"){
    new TestSets {
      var c1  = union(s1,s2)
      var all = union(c1, s3)

      var filteredNot1 = filter(all, el => el != 1)
      assert(contains(filteredNot1, 2), "Filter 1")
      assert(contains(filteredNot1, 3), "Filter 2")
      assert(!contains(filteredNot1, 1), "Filter 3")

      val filteredGt2 = filter(all, el => el > 2)
      assert(contains(filteredGt2, 3), "Filter 4")
      assert(!contains(filteredGt2, 1), "Filter 5")
      assert(!contains(filteredGt2, 2), "Filter 6")
    }
  }

  test("filter unpassed test"){
    new TestSets {
      val testInitial = union(union(union(union(s1, s3), s4), s5), s1000)
      var test = filter(testInitial, _ < 5)
      assert(contains(test, 1), "Unpassed 1")
      assert(contains(test, 3), "Unpassed 1")
      assert(contains(test, 4), "Unpassed 1")
      assert(!contains(test, 2), "Unpassed 1")
      assert(!contains(test, 0), "Unpassed 1")
      assert(!contains(test, -1), "Unpassed 1")

      var result = List(1,3,4)

      (-bound until bound).foreach { x =>
        if (result.contains(x)) assert(contains(test, x), "this element must be there dude")
        else assert(!contains(test, x), "this element can't be there dude")
      }
    }
  }

  test("forall tests if a condition is true for all (bounded) elements within a set"){
    new TestSets {
      var c1  = union(s1,s2)
      var all = union(c1, s3)

      assert(forall(all, _ > 0), "Forall 1")
      assert(!forall(all, _ < 0), "Forall 2")
      assert(forall(_ == 1, _ > 0), "Forall 3")
    }
  }

  test("exists tests if a condition is true for at least one (bounded) element within a set"){
    new TestSets {
      var c1  = union(s1,s2)
      var all = union(c1, s3)

      assert(exists(all, _ == 1), "Exists 1")
      assert(!exists(all, _ == 0), "Exists 2")
    }
  }

  test("maps all values of a set to f"){
    new TestSets {
      var origin = union(s1,s2)
      var target = union(s2,s3)

      var result = map(origin, _ + 1)
      assert(forall(target, contains(result, _)), "Map 1")
    }
  }
}
