import org.scalatest.FunSuite
import ro.upb.nrs.sl._
import ro.upb.nrs.sl.NumberRepresentationSystemConversions._

class oldPosit_Tests extends FunSuite{

  numberRepresentationSystemWorkingType = oldPosit_NR(oldPosit.default_exponent_size, oldPosit.default_size, oldPosit.default_rounding)

  test("toString ro.upb.nrs.sl.oldPosit") {
    assert(oldPosit(2).toString.equals("2"))
  }

  test("Addition ro.upb.nrs.sl.oldPosit") {
    assert(oldPosit(2.0d) + oldPosit(3.0d) == oldPosit(5.0d))
    assert(oldPosit(2) + oldPosit(3) == oldPosit(5))
  }

  test("Subraction ro.upb.nrs.sl.oldPosit") {
    assert(oldPosit(3.0d) - oldPosit(2.0d) == oldPosit(1.0d))
    assert(oldPosit(3) - oldPosit(2) == oldPosit(1))
  }

  test("Mul ro.upb.nrs.sl.oldPosit") {
    assert(oldPosit(3.0d) * oldPosit(2.0d) == oldPosit(6.0d))
    assert(oldPosit(3) * oldPosit(2) == oldPosit(6))
  }

  test("Div ro.upb.nrs.sl.oldPosit") {
    assert(oldPosit(6.0d) / oldPosit(2.0d) == oldPosit(3.0d))
    assert(oldPosit(6) / oldPosit(2) == oldPosit(3))
  }

  test("Pow ro.upb.nrs.sl.oldPosit") {
    assert(oldPosit(2.0d).pow(2) == oldPosit(4.0d))
    assert(oldPosit(6).pow(2) == oldPosit(36))
  }

  test("Nqrt ro.upb.nrs.sl.oldPosit") {
    assert(oldPosit(16.0d).nqrt(2) == oldPosit(4.0d))
    assert(oldPosit(8).nqrt(3) == oldPosit(2))
  }

  test("Abs ro.upb.nrs.sl.oldPosit") {
    assert(oldPosit(-16.0d).abs== oldPosit(16.0d))
    assert(oldPosit(-8).abs == oldPosit(8))
  }

  test("Signum ro.upb.nrs.sl.oldPosit") {
    assert(oldPosit(-16.0d).signum== oldPosit(-1.0d))
    assert(oldPosit(8).signum == oldPosit(1))
  }

  test("Equals ro.upb.nrs.sl.oldPosit") {
    assert(oldPosit(16.0d) == oldPosit(16.0d))
    assert(oldPosit(16) == oldPosit(16))
  }

  test("Complex ro.upb.nrs.sl.oldPosit") {
    // Asta trebuie fixata ca nici suma cu numere mici nu merge
    val x = oldPosit(1135984.7834d)
    val y = oldPosit(3423456.11345d)

    assert(oldPosit(11) + oldPosit(34)==oldPosit(45))

    val sum = x + y
    assert(sum > oldPosit(4559440.0d))
    val dif = x - y
    assert(dif == oldPosit(-2287471.0d))
    val mul = oldPosit(11359.7834d) * oldPosit(34234.11345d)
    assert(mul == oldPosit(388892113.0d))
    val div = oldPosit(11359.7834d) / oldPosit(34234.11345d)
    assert(div >= oldPosit(0.331d))
    val pow = oldPosit(11359.7834d).pow(2)
    assert(pow >= oldPosit(129044678.0d))
    val root = x.nqrt(2)
    assert(root >= oldPosit(1065.825d))
    val nthroot = x.nqrt(3)
    assert(nthroot >= oldPosit(104.341d))
  }


  test("Positive Zero only ro.upb.nrs.sl.oldPosit") {
    val positiveZero = oldPosit(0.0d)
    val negativeZero = oldPosit(0.0d) * oldPosit(-1.0d)
    assert(positiveZero == negativeZero)
    assert( (oldPosit(-1.0d) + oldPosit(1.0d)) == negativeZero )
    assert( (oldPosit(-1.0d) + oldPosit(1.0d)) == positiveZero )
    assert(positiveZero == (negativeZero * negativeZero) )
    //oldPosit has only one zero (positive)
    assert(positiveZero.sign == negativeZero.sign)
  }

  
}
