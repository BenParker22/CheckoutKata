import org.scalatest.FunSuite

class SetSuite extends FunSuite {

  val aRule = new Rule("A", 10, Some(3, 20))
  val bRule = new Rule("B", 20, Some(4, 50))
  val RuleSet = Vector(aRule, bRule)

  test("Price of 10 A's should be 70") {
    assert(aRule.findPrice(10, 0) === 70)
  }

  test("Price of 0 As should be 0") {
    assert(aRule.findPrice(0, 0) === 0)
  }

//  test("Entering C 20 should create such rule") {
//    assert(enterRule(Vector(), "C 20") === enterRule(Vector(new Rule("C", 20)), readNewRule))
//  }
//  test("Entering D 20 4 for 70 should create such rule") {
//    assert(createRuleFromString("D 20 4 for 70") === new Rule("D", 20, Some(4, 70)))
//  }
//
//  test("Entering D 20 5 should error") {
//    assert(createRuleFromString("D 20 5") == throw new Error("Unknown input"))
//  }
//
//  test("Calculate total price of cart 'A,B,A,B,B,B,A,A,B' should equal 100") {
//    assert(calculateTotalPrice(List("A", "B", "A", "B", "B", "B", "A", "A", "B"))) == 100
//  }

}
