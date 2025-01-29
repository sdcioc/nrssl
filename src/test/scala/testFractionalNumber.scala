import org.scalatest.FunSuite
import ro.upb.nrs.sl._
import ro.upb.nrs.sl.NumberRepresentationSystemConversions._

class FractionalNumber_Tests extends FunSuite {

  numberRepresentationSystemWorkingType = FractionalNumber_NR(FractionalNumber.default_size_numerator, FractionalNumber.default_size_denominator, FractionalNumber.default_rounding)

  test("Addition 1+2==3") {
    assert(1+FractionalNumber(2)==FractionalNumber(3))
  }

  test("Addition 1+2==3 v2") {
    assert(FractionalNumber(1)+2==FractionalNumber(3))
  }

  test("Subtraction ro.upb.nrs.sl.RationalNumber negative") {
    assert(FractionalNumber(1)-2==FractionalNumber(-1))
  }

  test("Subtraction ro.upb.nrs.sl.FractionalNumber") {
    assert(FractionalNumber(2.25d)-2==FractionalNumber(0.25d))
    assert(3 - FractionalNumber(2.25d)==FractionalNumber(0.75d))
  }

  test("Multiplication ro.upb.nrs.sl.FractionalNumber") {
    assert(FractionalNumber(3) * 2==FractionalNumber(6))
    assert(2 * FractionalNumber(3)==FractionalNumber(6))
    assert(FractionalNumber(0.25d) * FractionalNumber(0.25d)==FractionalNumber(0.0625d))
    assert(FractionalNumber(0.25d) * 2 ==FractionalNumber(0.5d))

    assert(FractionalNumber(2.3d) * FractionalNumber(2.0d)==FractionalNumber(4.6d))
    assert(FractionalNumber(0.25d) * FractionalNumber(0.3d)==FractionalNumber(0.075d))
  }

  test("Divison ro.upb.nrs.sl.FractionalNumber") {
    assert(FractionalNumber(3) / 2==FractionalNumber(1.5d))
    assert(FractionalNumber(3) / FractionalNumber(1.5d)==FractionalNumber(2))
    assert(3 / FractionalNumber(1.5d)==FractionalNumber(2))
  }

  /*
  test("Modulo ro.upb.nrs.sl.FractionalNumber") {
    assert(ro.upb.nrs.sl.FractionalNumber(3) % ro.upb.nrs.sl.FractionalNumber(2)==ro.upb.nrs.sl.FractionalNumber(1))
    assert(ro.upb.nrs.sl.FractionalNumber(3) % 2==ro.upb.nrs.sl.FractionalNumber(1))
    assert(2 % ro.upb.nrs.sl.FractionalNumber(3)==ro.upb.nrs.sl.FractionalNumber(1))
  }
  */

  test("Power ro.upb.nrs.sl.FractionalNumber") {
    assert(FractionalNumber(64).pow(FractionalNumber(2))==FractionalNumber(4096))
    assert(FractionalNumber(64).pow(2)==FractionalNumber(4096))
    assert(FractionalNumber(-10).pow(2)==FractionalNumber(100))
    assert(FractionalNumber(-10).pow(FractionalNumber(3))==FractionalNumber(-1000))
    assert(FractionalNumber(-10).pow(3)==FractionalNumber(-1000))

    assert(FractionalNumber(4.0d).pow(FractionalNumber(2.0d))==FractionalNumber(16))
    // TODO: find a number more fit ore use EPS
    //assert(ro.upb.nrs.sl.FractionalNumber(2.3d).pow(ro.upb.nrs.sl.FractionalNumber(2.0d))==ro.upb.nrs.sl.FractionalNumber(5.29d))
    //assert(ro.upb.nrs.sl.FractionalNumber(-2.3d).pow(ro.upb.nrs.sl.FractionalNumber(2.0d))==ro.upb.nrs.sl.FractionalNumber(5.29d))
    assert(FractionalNumber(4.0d).pow(FractionalNumber(-2.0d))==FractionalNumber(0.0625d))

  }

  test("Nqrt ro.upb.nrs.sl.FractionalNumber") {
    assert(FractionalNumber(64).nqrt(2)==FractionalNumber(8))
    assert(FractionalNumber(1000).nqrt(3)==FractionalNumber(10))
  }

  test("Abs ro.upb.nrs.sl.FractionalNumber") {
    assert(FractionalNumber(64).abs==FractionalNumber(64))
    assert(FractionalNumber(-64).abs==FractionalNumber(64))

    assert(FractionalNumber(2.3d).abs==FractionalNumber(2.3d))
    assert(FractionalNumber(-2.3d).abs==FractionalNumber(2.3d))
  }

  test("Signum ro.upb.nrs.sl.FractionalNumber") {
    assert(FractionalNumber(64).signum==FractionalNumber(1))
    assert(FractionalNumber(-64).signum==FractionalNumber(-1))

    assert(FractionalNumber(2.0d).signum==FractionalNumber(1))
    assert(FractionalNumber(-2.0d).signum==FractionalNumber(-1))

    assert(FractionalNumber(2.3d).signum==FractionalNumber(1))
    assert(FractionalNumber(-2.3d).signum==FractionalNumber(-1))
  }

  test("Complex ro.upb.nrs.sl.FractionalNumber") {

    val x = FractionalNumber(1135984.7834d, 64, 64, RoundEven)
    val y = FractionalNumber(3423456.11345d, 64, 64, RoundEven)

    assert(FractionalNumber(11) + FractionalNumber(34)==FractionalNumber(45))

    val sum = x + y
    assert(sum > FractionalNumber(4559440.0d, 64, 64, RoundEven))
    val dif = x - y
    assert(dif < FractionalNumber(-2287471.0d, 64, 64, RoundEven))
    val mul = FractionalNumber(11359.7834d, 64, 64, RoundEven) * FractionalNumber(34234.11345d, 64, 64, RoundEven)
    assert(mul > FractionalNumber(388892113.0d, 64, 64, RoundEven))
    val div = FractionalNumber(11359.7834d, 64, 64, RoundEven) / FractionalNumber(34234.11345d, 64, 64, RoundEven)
    assert(div > FractionalNumber(0.30d, 64, 64, RoundEven))
    val pow = FractionalNumber(11359.7834d, 64, 64, RoundEven).pow(2)
    assert(pow > FractionalNumber(129044678.0d, 64, 64, RoundEven))
    val root = x.nqrt(2)
    assert(root > FractionalNumber(1065.0d, 64, 64, RoundEven))
    val nthroot = x.nqrt(3)
    assert(nthroot > FractionalNumber(104.0d, 64, 64, RoundEven))
  }

  
  test("Double 1==1") {
    assert(FractionalNumber(1.0d)==FractionalNumber(1))
  }
  
  test("Float 1==1") {
    assert(FractionalNumber(1.0f)==FractionalNumber(1))
  }

  
  test("Double 1.5==3/2") {
    assert(FractionalNumber(1.5d)==FractionalNumber(3,2))
  }
  
  test("SQRT 16") {
    assert(FractionalNumber(16).sqrt==FractionalNumber(4))
  }
}