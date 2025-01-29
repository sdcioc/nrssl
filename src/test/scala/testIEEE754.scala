import org.scalatest.FunSuite
import ro.upb.nrs.sl._
import ro.upb.nrs.sl.NumberRepresentationSystemConversions._

class IEEE754_Tests extends FunSuite{

  numberRepresentationSystemWorkingType = IEEE754_NR(IEEE754.default_exponent_size, IEEE754.default_fraction_size, IEEE754.default_rounding)

  test("toString ro.upb.nrs.sl.IEEE754") {
    assert(IEEE754(2).toString.equals("2"))
  }

  test("Addition ro.upb.nrs.sl.IEEE754") {
    assert(IEEE754(2.0d) + IEEE754(3.0d) == IEEE754(5.0d))
    assert(IEEE754(2) + IEEE754(3) == IEEE754(5))
  }

  test("Subraction ro.upb.nrs.sl.IEEE754") {
    assert(IEEE754(3.0d) - IEEE754(2.0d) == IEEE754(1.0d))
    assert(IEEE754(3) - IEEE754(2) == IEEE754(1))
  }

  test("Mul ro.upb.nrs.sl.IEEE754") {
    assert(IEEE754(3.0d) * IEEE754(2.0d) == IEEE754(6.0d))
    assert(IEEE754(3) * IEEE754(2) == IEEE754(6))
  }

  test("Div ro.upb.nrs.sl.IEEE754") {
    assert(IEEE754(6.0d) / IEEE754(2.0d) == IEEE754(3.0d))
    assert(IEEE754(6) / IEEE754(2) == IEEE754(3))
  }

  test("Pow ro.upb.nrs.sl.IEEE754") {
    assert(IEEE754(2.0d).pow(2) == IEEE754(4.0d))
    assert(IEEE754(6).pow(2) == IEEE754(36))
  }

  test("Nqrt ro.upb.nrs.sl.IEEE754") {
    assert(IEEE754(16.0d).nqrt(2) == IEEE754(4.0d))
    assert(IEEE754(8).nqrt(3) == IEEE754(2))
  }

  test("Abs ro.upb.nrs.sl.IEEE754") {
    assert(IEEE754(-16.0d).abs== IEEE754(16.0d))
    assert(IEEE754(-8).abs == IEEE754(8))
  }

  test("Signum ro.upb.nrs.sl.IEEE754") {
    assert(IEEE754(-16.0d).signum== IEEE754(-1.0d))
    assert(IEEE754(8).signum == IEEE754(1))
  }

  test("Equals ro.upb.nrs.sl.IEEE754") {
    assert(IEEE754(16.0d) == IEEE754(16.0d))
    assert(IEEE754(16) == IEEE754(16))
  }

  test("Complex ro.upb.nrs.sl.IEEE754") {

    // Asta trebuie fixata ca nici suma cu numere mici nu merge
    val x = IEEE754(1135984.7834d)
    val y = IEEE754(3423456.11345d)

    assert(IEEE754(11) + IEEE754(34)==IEEE754(45))

    val sum = x + y
    assert(sum > IEEE754(4559440.0d))
    val dif = x - y
    assert(dif < IEEE754(-2287471.0d))
    val mul = IEEE754(11359.7834d) * IEEE754(34234.11345d)
    assert(mul >= IEEE754(388892096.0d))
    val div = IEEE754(11359.7834d) / IEEE754(34234.11345d)
    assert(div > IEEE754(0.30d))
    val pow = IEEE754(11359.7834d).pow(2)
    assert(pow >= IEEE754(129044672.0d))
    val root = x.nqrt(2)
    assert(root > IEEE754(1065.0d))
    val nthroot = x.nqrt(3)
    assert(nthroot > IEEE754(104.0d))
  }

  

  test("Positive Zero and Negative Zero ro.upb.nrs.sl.IEEE754") {
    val positiveZero = IEEE754(0.0d)
    val negativeZero = IEEE754(0.0d) * IEEE754(-1.0d)
    assert(positiveZero == negativeZero)
    assert( (IEEE754(-1.0d) + IEEE754(1.0d)) == negativeZero )
    assert( (IEEE754(-1.0d) + IEEE754(1.0d)) == positiveZero )
    assert(positiveZero == (negativeZero * negativeZero) )
    assert(positiveZero.value.sign != negativeZero.value.sign)
    assert(negativeZero.sqrt == negativeZero)
  }

  
  test("Positive Zero and Negative Zero inequalities ro.upb.nrs.sl.IEEE754") {
    val positiveZero = IEEE754(0.0d)
    val negativeZero = IEEE754(0.0d) * IEEE754(-1.0d)
    val positiveOne = IEEE754(1.0d)
    val negativeOne = IEEE754(1.0d) * IEEE754(-1.0d)
    assert(positiveZero == negativeZero)
    assert( negativeZero < positiveZero )
    assert( positiveZero > negativeZero )
    assert(positiveZero < positiveOne )
    assert(positiveZero < negativeOne)
    assert(negativeOne < negativeZero)
    assert(negativeZero < positiveOne)
    assert(negativeOne < positiveZero)
  }

  //TODO: INF tests

}
