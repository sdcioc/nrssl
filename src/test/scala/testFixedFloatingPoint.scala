import org.scalatest.FunSuite
import ro.upb.nrs.sl._
import ro.upb.nrs.sl.NumberRepresentationSystemConversions._

class FixedFloatingPoint_Tests extends FunSuite{

  numberRepresentationSystemWorkingType=FixedFloatingPoint_NR(FixedFloatingPoint.default_exponent_size, FixedFloatingPoint.default_fraction_size, FixedFloatingPoint.default_rounding)

  test("toString ro.upb.nrs.sl.FixedFloatingPoint") {
    assert(FixedFloatingPoint(2).toString.equals("2"))
    assert(FixedFloatingPoint(-3).toString.equals("-3"))
  }

  test("Addition ro.upb.nrs.sl.FixedFloatingPoint") {
    assert(FixedFloatingPoint(2.0d) + FixedFloatingPoint(3.0d) == FixedFloatingPoint(5.0d))
    assert(FixedFloatingPoint(2) + FixedFloatingPoint(3) == FixedFloatingPoint(5))
  }

  test("Subraction ro.upb.nrs.sl.FixedFloatingPoint") {
    assert(FixedFloatingPoint(3.0d) - FixedFloatingPoint(2.0d) == FixedFloatingPoint(1.0d))
    assert(FixedFloatingPoint(3) - FixedFloatingPoint(2) == FixedFloatingPoint(1))
  }

  test("Mul ro.upb.nrs.sl.FixedFloatingPoint") {
    assert(FixedFloatingPoint(3.0d) * FixedFloatingPoint(2.0d) == FixedFloatingPoint(6.0d))
    assert(FixedFloatingPoint(3) * FixedFloatingPoint(2) == FixedFloatingPoint(6))
  }

  test("Div ro.upb.nrs.sl.FixedFloatingPoint") {
    assert(FixedFloatingPoint(6.0d) / FixedFloatingPoint(2.0d) == FixedFloatingPoint(3.0d))
    assert(FixedFloatingPoint(6) / FixedFloatingPoint(2) == FixedFloatingPoint(3))
  }

  test("Pow ro.upb.nrs.sl.FixedFloatingPoint") {
    assert(FixedFloatingPoint(2.0d).pow(2) == FixedFloatingPoint(4.0d))
    assert(FixedFloatingPoint(6).pow(2) == FixedFloatingPoint(36))
  }

  test("Nqrt ro.upb.nrs.sl.FixedFloatingPoint") {
    assert(FixedFloatingPoint(16.0d).nqrt(2) == FixedFloatingPoint(4.0d))
    assert(FixedFloatingPoint(8).nqrt(3) == FixedFloatingPoint(2))
  }

  test("Abs ro.upb.nrs.sl.FixedFloatingPoint") {
    assert(FixedFloatingPoint(-16.0d).abs== FixedFloatingPoint(16.0d))
    assert(FixedFloatingPoint(-8).abs == FixedFloatingPoint(8))
  }

  test("Signum ro.upb.nrs.sl.FixedFloatingPoint") {
    assert(FixedFloatingPoint(-16.0d).signum== FixedFloatingPoint(-1.0d))
    assert(FixedFloatingPoint(8).signum == FixedFloatingPoint(1))
  }

  test("Equals ro.upb.nrs.sl.FixedFloatingPoint") {
    assert(FixedFloatingPoint(16.0d) == FixedFloatingPoint(16.0d))
    assert(FixedFloatingPoint(16) == FixedFloatingPoint(16))
  }

  test("Complex ro.upb.nrs.sl.FixedFloatingPoint") {
    val x = FixedFloatingPoint(1135984.7834d)
    val y = FixedFloatingPoint(3423456.11345d)

    assert(FixedFloatingPoint(11) + FixedFloatingPoint(34)==FixedFloatingPoint(45))

    val sum = x + y
    assert(sum > FixedFloatingPoint(4559440.0d, 64, 16, RoundEven))
    val dif = x - y
    assert(dif < FixedFloatingPoint(-2287471.0d, 64, 16, RoundEven))
    val mul = FixedFloatingPoint(11359.7834d) * FixedFloatingPoint(34234.11345d)
    assert(mul > FixedFloatingPoint(388892113.0d, 64, 16, RoundEven))
    val div = FixedFloatingPoint(11359.7834d) / FixedFloatingPoint(34234.11345d)
    assert(div > FixedFloatingPoint(0.30d, 64, 16, RoundEven))
    val pow = FixedFloatingPoint(11359.7834d).pow(2)
    assert(pow > FixedFloatingPoint(129044678.0d, 64, 16, RoundEven))
    val root = x.nqrt(2)
    assert(root > FixedFloatingPoint(1065.0d, 64, 16, RoundEven))
    val nthroot = x.nqrt(3)
    assert(nthroot > FixedFloatingPoint(104.0d, 64, 16, RoundEven))
  }

  
  test("Positive Zero and Negative Zero ro.upb.nrs.sl.FixedFloatingPoint") {
    val positiveZero = FixedFloatingPoint(0.0d)
    val negativeZero = FixedFloatingPoint(0.0d) * FixedFloatingPoint(-1.0d)
    assert(positiveZero == negativeZero)
    assert( (FixedFloatingPoint(-1.0d) + FixedFloatingPoint(1.0d)) == negativeZero )
    assert( (FixedFloatingPoint(-1.0d) + FixedFloatingPoint(1.0d)) == positiveZero )
    assert(positiveZero == (negativeZero * negativeZero) )
    assert(positiveZero.value.sign != negativeZero.value.sign)
    assert(negativeZero.sqrt == negativeZero)
  }


}
