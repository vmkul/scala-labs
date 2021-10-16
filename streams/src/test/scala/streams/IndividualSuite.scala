import individual.Func

class IndividualSuite extends munit.FunSuite {
  test("isDefinedAt functions correctly") {
    assert(!Func.functionCi.isDefinedAt(-10))
    assert(!Func.functionCi.isDefinedAt(1))
    assert(Func.functionCi.isDefinedAt(1.5))
    assert(Func.functionCi.isDefinedAt(2))
    assert(Func.functionCi.isDefinedAt(10))
  }

  test("Correct function values") {
    assertEquals(Func.functionCi(1.1), 1.1)
    assertEquals(Func.functionCi(2), 156.0)
    assertEquals(Func.functionCi(12.5), 975.0)
  }
}
