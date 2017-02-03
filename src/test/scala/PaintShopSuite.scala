package paint

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import PaintShop._

@RunWith(classOf[JUnitRunner])
class PaintShopSuite extends FunSuite {

  trait TestSets {
    val a = Seq[Colour](new Colour(1, 'M'), new Colour(3,'G'), new Colour(5,'G')) sortBy(c => (c.finish, c.code))
    val b = Seq[Colour](new Colour(2, 'G'), new Colour(3,'M'), new Colour(4,'G')) sortBy(c => (c.finish, c.code))
    val c = Seq[Colour](new Colour(5,'M'))

    val d = Seq[Colour](new Colour(1,'M'))
    val e = Seq[Colour](new Colour(1,'G'))

    val f = Seq[Colour](new Colour(1,'G'), new Colour(2,'M')) sortBy(c => (c.finish, c.code))

    val example1 = Map[Int, Preferences](1 -> a, 2 -> b, 3 -> c)
    val example2 = Map[Int, Preferences](1 -> d, 2 -> e)
    val example4 = Map[Int, Preferences](1 -> d, 2 -> f)

    val example1File = "src/test/resources/example1.txt"
    val example2File = "src/test/resources/example2.txt"
    val example3File = "src/test/resources/example3.txt"
    val example4File = "src/test/resources/example4.txt"
  }

  test("Colour") {
    assert(new Colour(1, 'M') != new Colour(1, 'G'))
    assert(new Colour(1, 'M') === new Colour(1, 'M'))
  }


  test("parseLine") {
    val parsed = PaintShop.parseLine("1 M 3 G 5 G")
    assert(parsed.length === 3)
    assert(parsed(0) === new Colour(1, 'M'))

    try {
      PaintShop.parseLine("")
      fail()
    }
    catch {
      case _: Exception => // Expected, so continue
    }

    try {
      PaintShop.parseLine("1 M 3")
      fail()
    }
    catch {
      case _: Exception => // Expected, so continue
    }

    try {
      PaintShop.parseLine("1 r")
      fail()
    }
    catch {
      case _: Exception => // Expected, so continue
    }
  }

  test("parseFile") {
    new TestSets {
      val results = PaintShop.parseFile(example1File)
      assert(results._1 === 5)
      val customerPreferences = results._2
      assert(customerPreferences.size == 3)
      assert(customerPreferences(1).head == new Colour(3, 'G'))
    }
  }

  test("nextChoice") {
    new TestSets {
      val choice = PaintShop.nextChoice(example1)
      assert(choice.length === 3)
      assert(choice.head.customerID === 3)
      assert(choice.head.colour === new Colour(5, 'M'))
      assert(choice.head.remainingColours === 1)
      assert(PaintShop.nextChoice(Map.empty).isEmpty)
    }
  }

  test("isValidChoice") {
    val colour = new Choice(1, new Colour(1, 'M'), 1)
      assert(PaintShop.isValidChoice(Map.empty, colour))
      assert(PaintShop.isValidChoice(Map[Int, Char](1 -> 'M'), colour))
      assert(!PaintShop.isValidChoice(Map[Int, Char](1 -> 'G'), colour))
  }

  test("solve") {
    new TestSets {
      val results = PaintShop.solve(example1)
      assert(results.size === 3)
      assert(results(3) === 'G')
      assert(results(2) === 'G')
      assert(results(5) === 'M')

      try {
        PaintShop.solve(example2)
        fail()
      }
      catch {
        case e: Exception => assert(e.getMessage == "No solution exists")
      }

      val results4 = PaintShop.solve(example4)
      assert(results4.size === 2)
      println(results4)
      assert(results4(1) === 'M')
      assert(results4(2) === 'M')
    }
  }

  test("mapToString") {
    assert(PaintShop.mapToString(Map.empty, Set.empty) === "")
    assert(PaintShop.mapToString(Map.empty, (1 to 3).toSet) === "G G G")
    assert(PaintShop.mapToString(Map[Int, Char](2 -> 'M'), (1 to 3).toSet) === "G M G")
  }

  test("findOptimalBatch") {
    new TestSets {
      assert(PaintShop.findOptimalBatch(example1File) === "G G G G M")
      assert(PaintShop.findOptimalBatch(example2File) === "No solution exists")
      assert(PaintShop.findOptimalBatch(example3File) === "G M G M G")
      assert(PaintShop.findOptimalBatch(example4File) === "M M")
    }
  }

}
