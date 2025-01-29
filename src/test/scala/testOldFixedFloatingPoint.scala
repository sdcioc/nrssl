import org.scalatest.FunSuite
import ro.upb.nrs.sl._
import ro.upb.nrs.sl.NumberRepresentationSystemConversions._

class oldFixedFloatingPoint_Tests extends FunSuite{

  numberRepresentationSystemWorkingType=oldFixedFloatingPoint_NR(oldFixedFloatingPoint.default_exponent_size, oldFixedFloatingPoint.default_fraction_size, oldFixedFloatingPoint.default_rounding)

  test("toString ro.upb.nrs.sl.oldFixedFloatingPoint") {
    assert(oldFixedFloatingPoint(2).toString.equals("2"))
    assert(oldFixedFloatingPoint(-3).toString.equals("-3"))
  }

  test("Addition ro.upb.nrs.sl.oldFixedFloatingPoint") {
    assert(oldFixedFloatingPoint(2.0d) + oldFixedFloatingPoint(3.0d) == oldFixedFloatingPoint(5.0d))
    assert(oldFixedFloatingPoint(2) + oldFixedFloatingPoint(3) == oldFixedFloatingPoint(5))
  }

  test("Subraction ro.upb.nrs.sl.oldFixedFloatingPoint") {
    assert(oldFixedFloatingPoint(3.0d) - oldFixedFloatingPoint(2.0d) == oldFixedFloatingPoint(1.0d))
    assert(oldFixedFloatingPoint(3) - oldFixedFloatingPoint(2) == oldFixedFloatingPoint(1))
  }

  test("Mul ro.upb.nrs.sl.oldFixedFloatingPoint") {
    assert(oldFixedFloatingPoint(3.0d) * oldFixedFloatingPoint(2.0d) == oldFixedFloatingPoint(6.0d))
    assert(oldFixedFloatingPoint(3) * oldFixedFloatingPoint(2) == oldFixedFloatingPoint(6))
  }

  test("Div ro.upb.nrs.sl.oldFixedFloatingPoint") {
    assert(oldFixedFloatingPoint(6.0d) / oldFixedFloatingPoint(2.0d) == oldFixedFloatingPoint(3.0d))
    assert(oldFixedFloatingPoint(6) / oldFixedFloatingPoint(2) == oldFixedFloatingPoint(3))
  }

  test("Pow ro.upb.nrs.sl.oldFixedFloatingPoint") {
    assert(oldFixedFloatingPoint(2.0d).pow(2) == oldFixedFloatingPoint(4.0d))
    assert(oldFixedFloatingPoint(6).pow(2) == oldFixedFloatingPoint(36))
  }

  test("Nqrt ro.upb.nrs.sl.oldFixedFloatingPoint") {
    assert(oldFixedFloatingPoint(16.0d).nqrt(2) == oldFixedFloatingPoint(4.0d))
    assert(oldFixedFloatingPoint(8).nqrt(3) == oldFixedFloatingPoint(2))
  }

  test("Abs ro.upb.nrs.sl.oldFixedFloatingPoint") {
    assert(oldFixedFloatingPoint(-16.0d).abs== oldFixedFloatingPoint(16.0d))
    assert(oldFixedFloatingPoint(-8).abs == oldFixedFloatingPoint(8))
  }

  test("Signum ro.upb.nrs.sl.oldFixedFloatingPoint") {
    assert(oldFixedFloatingPoint(-16.0d).signum== oldFixedFloatingPoint(-1.0d))
    assert(oldFixedFloatingPoint(8).signum == oldFixedFloatingPoint(1))
  }

  test("Equals ro.upb.nrs.sl.oldFixedFloatingPoint") {
    assert(oldFixedFloatingPoint(16.0d) == oldFixedFloatingPoint(16.0d))
    assert(oldFixedFloatingPoint(16) == oldFixedFloatingPoint(16))
  }

  test("Complex ro.upb.nrs.sl.oldFixedFloatingPoint") {
    val x = oldFixedFloatingPoint(1135984.7834d)
    val y = oldFixedFloatingPoint(3423456.11345d)

    assert(oldFixedFloatingPoint(11) + oldFixedFloatingPoint(34)==oldFixedFloatingPoint(45))

    val sum = x + y
    assert(sum > oldFixedFloatingPoint(4559440.0d, 64, 16, RoundEven))
    val dif = x - y
    assert(dif < oldFixedFloatingPoint(-2287471.0d, 64, 16, RoundEven))
    val mul = oldFixedFloatingPoint(11359.7834d) * oldFixedFloatingPoint(34234.11345d)
    assert(mul > oldFixedFloatingPoint(388892113.0d, 64, 16, RoundEven))
    val div = oldFixedFloatingPoint(11359.7834d) / oldFixedFloatingPoint(34234.11345d)
    assert(div > oldFixedFloatingPoint(0.30d, 64, 16, RoundEven))
    val pow = oldFixedFloatingPoint(11359.7834d).pow(2)
    assert(pow > oldFixedFloatingPoint(129044678.0d, 64, 16, RoundEven))
    val root = x.nqrt(2)
    assert(root > oldFixedFloatingPoint(1065.0d, 64, 16, RoundEven))
    val nthroot = x.nqrt(3)
    assert(nthroot > oldFixedFloatingPoint(104.0d, 64, 16, RoundEven))
  }

  
  test("Positive Zero and Negative Zero ro.upb.nrs.sl.oldFixedFloatingPoint") {
    val positiveZero = oldFixedFloatingPoint(0.0d)
    val negativeZero = oldFixedFloatingPoint(0.0d) * oldFixedFloatingPoint(-1.0d)
    assert(positiveZero == negativeZero)
    assert( (oldFixedFloatingPoint(-1.0d) + oldFixedFloatingPoint(1.0d)) == negativeZero )
    assert( (oldFixedFloatingPoint(-1.0d) + oldFixedFloatingPoint(1.0d)) == positiveZero )
    assert(positiveZero == (negativeZero * negativeZero) )
    assert(positiveZero.sign != negativeZero.sign)
    assert(negativeZero.sqrt == negativeZero)
  }


}
