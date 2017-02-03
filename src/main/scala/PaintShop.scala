package paint

object PaintShop {

  /**
    * Tha command line interface.
    *
    * @param args The file name.
    */
  def main(args: Array[String]) {
    if (args.length < 1)
      println("Pass in the file name")
    println(findOptimalBatch(args(0)))
  }

  private val matte = 'M'
  private val gloss = 'G'

  /**
    * Define a colour as the code plus the finish (gloss or matte).
    *
    * @param code An integer code
    * @param finish G for Gloss or M for Matte
    */
  class Colour(var code: Int, var finish: Char) {

    def canEqual(a: Any) = a.isInstanceOf[Colour]

    override def equals(that: Any): Boolean = that match {
        case that: Colour => that.canEqual(this) && this.code == that.code && this.finish == that.finish
        case _ => false
    }

    override def hashCode: Int = {
      return code * finish
    }

    override def toString = {
      code + " " + finish
    }
  }

  /**
    * Represents a single choice that can be made for a customer.
    *
    * @param customerID The customer's ID.
    * @param colour The colour.
    * @param remainingColours The number of remaining colours that can be used.
    */
  class Choice(var customerID: Int, var colour: Colour, var remainingColours: Int)

  /**
    * Holds a customers' preferred Colours.
    */
  type Preferences = Seq[Colour]

  /**
    * Parses a single line into the Preferences for a single customer.
    * If the line is poorly formatted an exception is thrown.
    *
    * @param line A line in the text file.
    * @return A Seq of Colours representing the customer's preference.
    */
  def parseLine(line: String): Preferences = {

    //parse a colour, throw an Exception if unexpected finish is found
    def parseColour(pair: Seq[String]): Colour = {
      val colour = new Colour(pair(0).toInt, pair(1).charAt(0))
      if (colour.finish != matte && colour.finish != gloss)
        throw new RuntimeException("Unexpected colour finish found: " + colour.finish)
      colour
    }

    val values = line.split(" ")
    if (values.length % 2 != 0)
      throw new RuntimeException("Badly formatted file. Unable to parse this line " + line)

    values grouped(2) map(g => parseColour(g)) toList
  }

  /**
    * Reads and parses a file into customer preferences,
    * each customer preference is sorted by colour code and then finish
    * i.e. matte last
    * If the file can't be read an exception is thrown.
    *
    * @param path The path of the file
    * @return A pair, the first element is the number of colours,
    *         the second element is Map with all customer preferences.
    */
  def parseFile(path: String): (Int, Map[Int, Preferences]) = {
    val source = io.Source.fromFile(path)
    try {
      var firstLine = true
      var colours = 0
      val customers = scala.collection.mutable.Map[Int, Preferences]()
      var customerID = 1
      for (line <- source.getLines) {
        //pull the number of colours from the first line
        if (firstLine) {
          colours = line.trim.toInt
          firstLine = false
        }
        else {
          customers += (customerID -> parseLine(line).sortBy(c => (c.finish, c.code)))
          customerID = customerID + 1
        }
      }
      (colours, customers.toMap)
    }
    finally {
      source.close
    }
  }

  /**
    * Generates the next set of ranked choices for each customer.
    *
    * @param preferences The remaining preferences for all of the customers.
    * @return The next choice per customer sorted by the number of remaining colours they have.
    */
  def nextChoice(preferences: Map[Int, Seq[Colour]]): Seq[Choice] = {
    val choices = for {
      k <- preferences.keys
    } yield new Choice(k, preferences(k).head, preferences(k).length)
    choices.toList sortBy(c => c.remainingColours)
  }

  /**
    * Checks if the choice is valid i.e. that it doesn't conflict with any
    * selected colours.
    *
    * @param colours The colours selected so far.
    * @param choice The choice to evaluate.
    * @return true if valid, false if it conflicts.
    */
  def isValidChoice(colours: Map[Int, Char], choice: Choice): Boolean = {
    !colours.contains(choice.colour.code) ||
      (colours.contains(choice.colour.code) && colours(choice.colour.code) == choice.colour.finish)
  }

  /**
    * Solves the problem of which batch of paint to produce. It does this by recursively selecting
    * non-conflicting colours for each customer. The preferences are ordered so the solution minimises the
    * amount of matte produced.
    *
    * One alternative approach would be to dynamically generate potential solutions
    * in ascending cost order and see if it is compatible with the given constraints.
    *
    * @param preferences The preferences of each customer.
    * @param colours An accumulator to store the colours.
    * @param choices A single colour option for each customer.
    * @return The solution it exists.
    */
  def solve(preferences: Map[Int, Seq[Colour]],
            colours: Map[Int, Char] = Map.empty,
            choices: Seq[Choice] = Seq.empty): Map[Int, Char] = {

    if (preferences.isEmpty) colours // the end point if a solution is found
    else {
      // generate the first colour for each customer and try to use it
      if (choices.isEmpty) solve(preferences, colours, nextChoice(preferences))
      else {
        val choice = choices.head

        // if it is the final choice for a customer try to add it or else fail
        if(choice.remainingColours == 1) {
          if (isValidChoice(colours, choice))
            solve(preferences - choice.customerID,
              colours + (choice.colour.code -> choice.colour.finish), choices.tail)
          else throw new RuntimeException("No solution exists")
        }
        else {
          // if the choice is valid add the colour and remove that customer's preferences
          // else pop the remaining choices for that customer and evaluate the next customer's choice
          if (isValidChoice(colours, choice))
            solve(preferences - choice.customerID,
              colours + (choice.colour.code -> choice.colour.finish), choices.tail)
          else solve(preferences + (choice.customerID -> preferences(choice.customerID).tail), colours, choices.tail)
        }
      }
    }
  }

  /**
    * Converts the solution into a string padding out any remaining colours as gloss
    * as this is the cheapest option.
    *
    * @param colours The colours for each customer
    * @param allColours The total set of customers
    * @return The formatted string of the colours.
    */
  def mapToString(colours: Map[Int, Char], allColours: Set[Int]): String = {

    //add any colours not chosen and sort the results
    val colourList = colours map {case (key, value) => new Colour(key, value)}
    val remainingColours = allColours filter(c => !colours.contains(c)) map(c => new Colour(c, gloss))
    (colourList ++ remainingColours).toList sortBy(c => c.code) map(c => c.finish) mkString(" ")
  }

  /**
    * Finds the optimal batch if one exists.
    *
    * @param path The path of a file containing the customers' orders.
    * @return The optimal batch if it exists r a message that no solution
    *         exists.
    */
  def findOptimalBatch(path: String): String = {
    try {
      // read the data
      val data = parseFile(path)

      //find a solution if it exists
      val result = solve(data._2)

      // format the results
      if (result.isEmpty) "No solution exists"
      else {
        // this assumes that colour codes are sequential integers from 1 to n (the number at the top of the file).
        val colours = (1 to data._1).toSet
        mapToString(result, colours)
      }
    }
    catch {
      case e: Exception => return e.getMessage
    }
  }
}





