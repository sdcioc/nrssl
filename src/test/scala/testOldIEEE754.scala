import org.scalatest.FunSuite
import ro.upb.nrs.sl._
import ro.upb.nrs.sl.NumberRepresentationSystemConversions._

class oldIEEE754_Tests extends FunSuite{

  numberRepresentationSystemWorkingType = oldIEEE754_NR(oldIEEE754.default_exponent_size, oldIEEE754.default_fraction_size, oldIEEE754.default_rounding)

  test("toString ro.upb.nrs.sl.oldIEEE754") {
    assert(oldIEEE754(2).toString.equals("2"))
  }

  test("Addition ro.upb.nrs.sl.oldIEEE754") {
    assert(oldIEEE754(2.0d) + oldIEEE754(3.0d) == oldIEEE754(5.0d))
    assert(oldIEEE754(2) + oldIEEE754(3) == oldIEEE754(5))
  }

  test("Subraction ro.upb.nrs.sl.oldIEEE754") {
    assert(oldIEEE754(3.0d) - oldIEEE754(2.0d) == oldIEEE754(1.0d))
    assert(oldIEEE754(3) - oldIEEE754(2) == oldIEEE754(1))
  }

  test("Mul ro.upb.nrs.sl.oldIEEE754") {
    assert(oldIEEE754(3.0d) * oldIEEE754(2.0d) == oldIEEE754(6.0d))
    assert(oldIEEE754(3) * oldIEEE754(2) == oldIEEE754(6))
  }

  test("Div ro.upb.nrs.sl.oldIEEE754") {
    assert(oldIEEE754(6.0d) / oldIEEE754(2.0d) == oldIEEE754(3.0d))
    assert(oldIEEE754(6) / oldIEEE754(2) == oldIEEE754(3))
  }

  test("Pow ro.upb.nrs.sl.oldIEEE754") {
    assert(oldIEEE754(2.0d).pow(2) == oldIEEE754(4.0d))
    assert(oldIEEE754(6).pow(2) == oldIEEE754(36))
  }

  test("Nqrt ro.upb.nrs.sl.oldIEEE754") {
    assert(oldIEEE754(16.0d).nqrt(2) == oldIEEE754(4.0d))
    assert(oldIEEE754(8).nqrt(3) == oldIEEE754(2))
  }

  test("Abs ro.upb.nrs.sl.oldIEEE754") {
    assert(oldIEEE754(-16.0d).abs== oldIEEE754(16.0d))
    assert(oldIEEE754(-8).abs == oldIEEE754(8))
  }

  test("Signum ro.upb.nrs.sl.oldIEEE754") {
    assert(oldIEEE754(-16.0d).signum== oldIEEE754(-1.0d))
    assert(oldIEEE754(8).signum == oldIEEE754(1))
  }

  test("Equals ro.upb.nrs.sl.oldIEEE754") {
    assert(oldIEEE754(16.0d) == oldIEEE754(16.0d))
    assert(oldIEEE754(16) == oldIEEE754(16))
  }

  test("Complex ro.upb.nrs.sl.oldIEEE754") {

    // Asta trebuie fixata ca nici suma cu numere mici nu merge
    val x = oldIEEE754(1135984.7834d)
    val y = oldIEEE754(3423456.11345d)

    assert(oldIEEE754(11) + oldIEEE754(34)==oldIEEE754(45))

    val sum = x + y
    assert(sum > oldIEEE754(4559440.0d))
    val dif = x - y
    assert(dif < oldIEEE754(-2287471.0d))
    val mul = oldIEEE754(11359.7834d) * oldIEEE754(34234.11345d)
    assert(mul >= oldIEEE754(388892096.0d))
    val div = oldIEEE754(11359.7834d) / oldIEEE754(34234.11345d)
    assert(div > oldIEEE754(0.30d))
    val pow = oldIEEE754(11359.7834d).pow(2)
    assert(pow >= oldIEEE754(129044672.0d))
    val root = x.nqrt(2)
    assert(root > oldIEEE754(1065.0d))
    val nthroot = x.nqrt(3)
    assert(nthroot > oldIEEE754(104.0d))
  }

  

  test("Positive Zero and Negative Zero ro.upb.nrs.sl.oldIEEE754") {
    val positiveZero = oldIEEE754(0.0d)
    val negativeZero = oldIEEE754(0.0d) * oldIEEE754(-1.0d)
    assert(positiveZero == negativeZero)
    assert( (oldIEEE754(-1.0d) + oldIEEE754(1.0d)) == negativeZero )
    assert( (oldIEEE754(-1.0d) + oldIEEE754(1.0d)) == positiveZero )
    assert(positiveZero == (negativeZero * negativeZero) )
    assert(positiveZero.sign != negativeZero.sign)
    assert(negativeZero.sqrt == negativeZero)
  }

  //TODO: INF tests

}
