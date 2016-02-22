import scala.io.StdIn

object Main extends App {

  def readNewRule = {
    println("enter a new price rule, 'begin' to start checkout or 'clear' to start again\n" +
      "A price rule is of the format: Letter Unit price Special price\n" +
      "Each seperated by a space.\n" +
      "e.g. 'A 30 3 for 70'\n" +
      "or 'A 30' if there is no special price")
    
    StdIn.readLine().toLowerCase
  }

  def enterRule(acc: Vector[Rule], readString: => String): Unit = {

    // TODO Don't allow multiple entries of the same Key
    def createRuleFromString(input: String): Rule = {
      val splitString = input.split(' ')

      splitString.length match {
        case 2 => new Rule(splitString(0), splitString(1).toInt) // A 20 => Rule(A, 20)
        case 5 => new Rule(splitString(0), splitString(1).toInt, Some(splitString(2).toInt, splitString(4).toInt)) // B 20 3 for 50 => Rule(B, 20, Some(3,50))
        case _ => throw new Error("Unknown input")
      }
    }

    readString match {
      case "begin" => addCheckoutItem(List(), acc)
      case "clear" => enterRule(Vector(), readNewRule)

      case rule: String => enterRule(acc :+ createRuleFromString(rule), readNewRule )
      case _ => enterRule(acc, readNewRule ) //TODO Give a message to indicate poor input
    }
  }

  def addCheckoutItem(acc: List[String], RuleSet: Vector[Rule]): Unit = {

    println ("Current cart:\n" + acc.mkString)
    println ("Current total: " + calculateTotalPrice(acc).toString)
    println("add new item by typing a letter, 'restart' to start from the beginning or 'clear' to start again")

    // TODO don't allow people to add an item which has no rule
    StdIn.readLine().toLowerCase match {
      case "clear" => addCheckoutItem(List(), RuleSet)
      case "restart" => enterRule(Vector(), readNewRule)
      case x: String => addCheckoutItem(x :: acc , RuleSet)
      case _ => throw new Error("unknown input") // TODO make this not exit the programme
    }

    def calculateTotalPrice( scannedItems: List[String]): Int = {

      val groupedInput = scannedItems.groupBy(identity).map(y => (y._1, y._2.size))

      def itemToPrice(keyAndtotalPair: (String, Int)): Int = {
        val ruleForItem = RuleSet.find(rule => rule.key == keyAndtotalPair._1)

        ruleForItem match {
          case Some(rule) => rule.findPrice(keyAndtotalPair._2, 0)
          case _ => throw new Error("No rule for item found")
        }
      }
      val listOfValuations = groupedInput.map(x => itemToPrice(x))

      listOfValuations.sum
    }
  }

  enterRule(Vector(), StdIn.readLine().toLowerCase)
}

class Rule(val key: String, unitPrice: Int, specialPrice: Option[(Int, Int)] = None) {

  def findPrice(numberOfItems:  Int, acc: Int): Int = {

    if (numberOfItems <= 0) acc

    else specialPrice match {
      case Some((offerNumberOfItems, offerPrice)) =>
        if (numberOfItems >= offerNumberOfItems) findPrice(numberOfItems - offerNumberOfItems, acc + offerPrice)
        else findPrice(numberOfItems - 1, acc + unitPrice)

      case None => findPrice(numberOfItems - 1, acc + unitPrice)
    }
  }
}
