import org.scalatest.FunSuite
import ro.upb.nrs.sl._
import ro.upb.nrs.sl.NumberRepresentationSystemConversions._
class RationalNumber_Tests extends FunSuite {

  numberRepresentationSystemWorkingType = RationalNumber_NR

  test("Addition 1+2==3") {
    assert(1+RationalNumber(2)==RationalNumber(3))
  }

  test("Addition 1+2==3 v2") {
    assert(RationalNumber(1)+2==RationalNumber(3))
  }

  test("Subtraction ro.upb.nrs.sl.RationalNumber negative") {
    assert(RationalNumber(1)-2==RationalNumber(-1))
  }

  test("Subtraction ro.upb.nrs.sl.RationalNumber") {
    assert(RationalNumber(2.25d)-2==RationalNumber(0.25d))
    assert(3 - RationalNumber(2.25d)==RationalNumber(0.75d))
  }

  test("Multiplication ro.upb.nrs.sl.RationalNumber") {
    assert(RationalNumber(3) * 2==RationalNumber(6))
    assert(2 * RationalNumber(3)==RationalNumber(6))
    assert(RationalNumber(0.25d) * RationalNumber(0.25d)==RationalNumber(0.0625d))
    assert(RationalNumber(0.25d) * 2 ==RationalNumber(0.5d))
  }

  test("Divison ro.upb.nrs.sl.RationalNumber") {
    assert(RationalNumber(3) / 2==RationalNumber(1.5d))
    assert(RationalNumber(3) / RationalNumber(1.5d)==RationalNumber(2))
    assert(3 / RationalNumber(1.5d)==RationalNumber(2))
  }

  /*
  test("Modulo ro.upb.nrs.sl.RationalNumber") {
    assert(ro.upb.nrs.sl.RationalNumber(3) % ro.upb.nrs.sl.RationalNumber(2)==ro.upb.nrs.sl.RationalNumber(1))
    assert(ro.upb.nrs.sl.RationalNumber(3) % 2==ro.upb.nrs.sl.RationalNumber(1))
    assert(2 % ro.upb.nrs.sl.RationalNumber(3)==ro.upb.nrs.sl.RationalNumber(1))
  }
  */

  test("Power ro.upb.nrs.sl.RationalNumber") {
    assert(RationalNumber(64).pow(RationalNumber(2))==RationalNumber(4096))
    assert(RationalNumber(64).pow(2)==RationalNumber(4096))
    assert(RationalNumber(-10).pow(2)==RationalNumber(100))
    assert(RationalNumber(-10).pow(RationalNumber(3))==RationalNumber(-1000))
    assert(RationalNumber(-10).pow(3)==RationalNumber(-1000))

    assert(RationalNumber(4.0d).pow(RationalNumber(2.0d))==RationalNumber(16))

    val temp = RationalNumber(2.3d).pow(RationalNumber(2.0d))
    assert(temp > RationalNumber(5.289d) && temp < RationalNumber(5.291d))

    assert(RationalNumber(-4.0d).pow(RationalNumber(2.0d))==RationalNumber(16.0d))
    assert(RationalNumber(4.0d).pow(RationalNumber(-2.0d))==RationalNumber(0.0625d))
    assert(RationalNumber(-4.0d).pow(RationalNumber(3.0d))==RationalNumber(-64.0d))

  }

  test("Nqrt ro.upb.nrs.sl.RationalNumber") {
    assert(RationalNumber(64).nqrt(2)==RationalNumber(8))
    assert(RationalNumber(1000).nqrt(3)==RationalNumber(10))
  }

  test("Abs ro.upb.nrs.sl.RationalNumber") {
    assert(RationalNumber(64).abs==RationalNumber(64))
    assert(RationalNumber(-64).abs==RationalNumber(64))

    assert(RationalNumber(2.3d).abs==RationalNumber(2.3d))
    assert(RationalNumber(-2.3d).abs==RationalNumber(2.3d))
  }

  test("Signum ro.upb.nrs.sl.RationalNumber") {
    assert(RationalNumber(64).signum==RationalNumber(1))
    assert(RationalNumber(-64).signum==RationalNumber(-1))

    assert(RationalNumber(2.0d).signum==RationalNumber(1))
    assert(RationalNumber(-2.0d).signum==RationalNumber(-1))

    assert(RationalNumber(2.3d).signum==RationalNumber(1))
    assert(RationalNumber(-2.3d).signum==RationalNumber(-1))
  }

  test("Complex ro.upb.nrs.sl.RationalNumber") {

    val x = RationalNumber(1135984.7834d)
    val y = RationalNumber(3423456.11345d)

    assert(RationalNumber(11) + RationalNumber(34)==RationalNumber(45))

    val sum = x + y
    assert(sum > RationalNumber(4559440.0d))
    val dif = x - y
    assert(dif < RationalNumber(-2287471.0d))
    val mul = RationalNumber(11359.7834d) * RationalNumber(34234.11345d)
    assert(mul > RationalNumber(388892113.0d))
    val div = RationalNumber(11359.7834d) / RationalNumber(34234.11345d)
    assert(div > RationalNumber(0.30d))
    val pow = RationalNumber(11359.7834d).pow(2)
    assert(pow > RationalNumber(129044678.0d))
    val root = x.nqrt(2)
    assert(root > RationalNumber(1065.0d))
    val nthroot = x.nqrt(3)
    assert(nthroot > RationalNumber(104.0d))
  }

  
  test("Double 1==1") {
    assert(RationalNumber(1.0d)==RationalNumber(1))
  }
  
  test("Float 1==1") {
    assert(RationalNumber(1.0f)==RationalNumber(1))
  }

  
  test("Double 1.5==3/2") {
    assert(RationalNumber(1.5d)==RationalNumber(3,2))
  }
  
  test("SQRT 16") {
    assert(RationalNumber(16).sqrt==RationalNumber(4))
  }

  
  test("Positive Zero and Negative Zero ro.upb.nrs.sl.RationalNumber") {
    val positiveZero = RationalNumber(0.0d)
    val negativeZero = RationalNumber(0.0d) * RationalNumber(-1.0d)
    assert(positiveZero == negativeZero)
    assert( (RationalNumber(-1.0d) + RationalNumber(1.0d)) == negativeZero )
    assert( (RationalNumber(-1.0d) + RationalNumber(1.0d)) == positiveZero )
    assert(positiveZero == (negativeZero * negativeZero) )
    //RationalNumber has only one zero (positive)
    assert(positiveZero.numerator.sign == negativeZero.numerator.sign)
  }

}