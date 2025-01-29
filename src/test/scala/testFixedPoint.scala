import org.scalatest.FunSuite
import ro.upb.nrs.sl._
import ro.upb.nrs.sl.NumberRepresentationSystemConversions._
class FixedPoint_Tests extends FunSuite{

  numberRepresentationSystemWorkingType = FixedPoint_NR(FixedPoint.default_size, FixedPoint.default_fraction_size, FixedPoint.default_rounding)

  test("toString ro.upb.nrs.sl.FixedPoint") {
    assert(FixedPoint(2).toString.equals("2"))
    assert(FixedPoint(100000, 32, 2, RoundEven).toString.equals("100000"))
  }

  test("Addition ro.upb.nrs.sl.FixedPoint") {
    assert(FixedPoint(2.0d) + FixedPoint(3.0d) == FixedPoint(5.0d))
    assert(FixedPoint(2) + FixedPoint(3) == FixedPoint(5))
  }

  test("Subraction ro.upb.nrs.sl.FixedPoint") {
    assert(FixedPoint(3.0d) - FixedPoint(2.0d) == FixedPoint(1.0d))
    assert(FixedPoint(3) - FixedPoint(2) == FixedPoint(1))
  }

  test("Mul ro.upb.nrs.sl.FixedPoint") {
    assert(FixedPoint(3.0d) * FixedPoint(2.0d) == FixedPoint(6.0d))
    assert(FixedPoint(3) * FixedPoint(2) == FixedPoint(6))
  }

  test("Div ro.upb.nrs.sl.FixedPoint") {
    assert(FixedPoint(6.0d) / FixedPoint(2.0d) == FixedPoint(3.0d))
    assert(FixedPoint(6) / FixedPoint(2) == FixedPoint(3))
  }

  test("Pow ro.upb.nrs.sl.FixedPoint") {
    assert(FixedPoint(2.0d).pow(2) == FixedPoint(4.0d))
    assert(FixedPoint(6).pow(2) == FixedPoint(36))
  }

  test("Nqrt ro.upb.nrs.sl.FixedPoint") {
    assert(FixedPoint(16.0d).nqrt(2) == FixedPoint(4.0d))
    assert(FixedPoint(8).nqrt(3) == FixedPoint(2))
  }

  test("Abs ro.upb.nrs.sl.FixedPoint") {
    assert(FixedPoint(-16.0d).abs== FixedPoint(16.0d))
    assert(FixedPoint(-8).abs == FixedPoint(8))
  }

  test("Signum ro.upb.nrs.sl.FixedPoint") {
    assert(FixedPoint(-16.0d).signum== FixedPoint(-1.0d))
    assert(FixedPoint(8).signum == FixedPoint(1))
  }

  test("Equals ro.upb.nrs.sl.FixedPoint") {
    assert(FixedPoint(16.0d) == FixedPoint(16.0d))
    assert(FixedPoint(16) == FixedPoint(16))
  }

  test("Complex small numbers ro.upb.nrs.sl.FixedPoint") {
    val x = FixedPoint(0.2456d)
    val y = FixedPoint(0.12344545d)

    val sum = x + y
    assert(sum > 0.369d && sum < 0.37d)
    val dif = x - y
    assert(dif > 0.122d && dif < 0.123d)
    val mul = x * y
    assert(mul > 0.030d && mul < 0.031d)
    val div = x / y
    assert(div > 1.989d && div < 1.99d)
    val pow = x.pow(2)
    assert(pow > 0.06d && pow < 0.061d)
    val root = x.nqrt(2)
    assert(root > 0.495d && root < 0.496d)
  }

  test("Complex ro.upb.nrs.sl.FixedPoint") {
    val x = FixedPoint(1135984.7834d, 64, 16, RoundEven)
    val y = FixedPoint(3423456.11345d, 64, 16, RoundEven)


    val sum = x + y
    assert(sum > FixedPoint(4559440.0d, 64, 16, RoundEven))
    val dif = x - y
    assert(dif < FixedPoint(-2287471.0d, 64, 16, RoundEven))
    val mul = FixedPoint(11359.7834d) * FixedPoint(34234.11345d)
    assert(mul > FixedPoint(388892113.0d, 64, 16, RoundEven))
    val div = FixedPoint(11359.7834d) / FixedPoint(34234.11345d)
    assert(div > FixedPoint(0.30d, 64, 16, RoundEven))
    val pow = FixedPoint(11359.7834d).pow(2)
    assert(pow > FixedPoint(129044678.0d, 64, 16, RoundEven))
    val root = x.nqrt(2)
    assert(root > FixedPoint(1065.0d, 64, 16, RoundEven))
    val nthroot = x.nqrt(3)
    assert(nthroot > FixedPoint(104.0d, 64, 16, RoundEven))
  }


}
