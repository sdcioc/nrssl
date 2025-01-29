import org.scalatest.FunSuite
import ro.upb.nrs.sl._
import ro.upb.nrs.sl.NumberRepresentationSystemConversions._

class Morris_Tests extends FunSuite{

  numberRepresentationSystemWorkingType = Morris_NR(Morris.default_g_size, Morris.default_size, Morris.default_rounding)

  test("toString ro.upb.nrs.sl.Morris") {
    assert(Morris(2).toString.equals("2"))
  }

  test("Addition ro.upb.nrs.sl.Morris") {
    assert(Morris(2.0d) + Morris(3.0d) == Morris(5.0d))
    assert(Morris(2) + Morris(3) == Morris(5))
  }

  test("Subraction ro.upb.nrs.sl.Morris") {
    assert(Morris(3.0d) - Morris(2.0d) == Morris(1.0d))
    assert(Morris(3) - Morris(2) == Morris(1))
  }

  test("Mul ro.upb.nrs.sl.Morris") {
    assert(Morris(3.0d) * Morris(2.0d) == Morris(6.0d))
    assert(Morris(3) * Morris(2) == Morris(6))
  }

  test("Div ro.upb.nrs.sl.Morris") {
    assert(Morris(6.0d) / Morris(2.0d) == Morris(3.0d))
    assert(Morris(6) / Morris(2) == Morris(3))
  }

  test("Pow ro.upb.nrs.sl.Morris") {
    assert(Morris(2.0d).pow(2) == Morris(4.0d))
    assert(Morris(6).pow(2) == Morris(36))
  }

  test("Nqrt ro.upb.nrs.sl.Morris") {
    assert(Morris(16.0d).nqrt(2) == Morris(4.0d))
    assert(Morris(8).nqrt(3) == Morris(2))
  }

  test("Abs ro.upb.nrs.sl.Morris") {
    assert(Morris(-16.0d).abs== Morris(16.0d))
    assert(Morris(-8).abs == Morris(8))
  }

  test("Signum ro.upb.nrs.sl.Morris") {
    assert(Morris(-16.0d).signum== Morris(-1.0d))
    assert(Morris(8).signum == Morris(1))
  }

  test("Equals ro.upb.nrs.sl.Morris") {
    assert(Morris(16.0d) == Morris(16.0d))
    assert(Morris(16) == Morris(16))
  }

  test("Complex ro.upb.nrs.sl.Morris") {
    // Asta trebuie fixata ca nici suma cu numere mici nu merge
    val x = Morris(1135984.7834d)
    val y = Morris(3423456.11345d)

    assert(Morris(11) + Morris(34)==Morris(45))

    val sum = x + y
    assert(sum >= Morris(4559440.0d))
    val dif = x - y
    assert(dif == Morris(-2287471.0d))
    val mul = Morris(11359.7834d) * Morris(34234.11345d)
    assert(mul == Morris(388892113.0d))
    val div = Morris(11359.7834d) / Morris(34234.11345d)
    assert(div >= Morris(0.331d))
    val pow = Morris(11359.7834d).pow(2)
    assert(pow >= Morris(129044678.0d))
    val root = x.nqrt(2)
    assert(root >= Morris(1065.825d))
    val nthroot = x.nqrt(3)
    assert(nthroot >= Morris(104.341d))
  }


  test("Positive Zero only ro.upb.nrs.sl.Morris") {
    val positiveZero = Morris(0.0d)
    val negativeZero = Morris(0.0d) * Morris(-1.0d)
    assert(positiveZero == negativeZero)
    assert( (Morris(-1.0d) + Morris(1.0d)) == negativeZero )
    assert( (Morris(-1.0d) + Morris(1.0d)) == positiveZero )
    assert(positiveZero == (negativeZero * negativeZero) )
    //Morris has only one zero (positive)
    assert(positiveZero.value.sign == negativeZero.value.sign)
  }

  
}
