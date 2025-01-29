import org.scalatest.FunSuite
import ro.upb.nrs.sl._
import ro.upb.nrs.sl.NumberRepresentationSystemConversions._

class InfiniteFloatingPoint_Tests extends FunSuite{

  numberRepresentationSystemWorkingType = InfiniteFloatingPoint_NR

  test("toString ro.upb.nrs.sl.InfiniteFloatingPoint") {
    // pare ca orice as printa cu toString da 0, iar cu toInternalString : 0 bitLength: 0 binary:0/2^16
    assert(InfiniteFloatingPoint(2).toString.equals("2"))
  }

  test("Addition ro.upb.nrs.sl.InfiniteFloatingPoint") {
    assert(InfiniteFloatingPoint(2.0d) + InfiniteFloatingPoint(3.0d) == InfiniteFloatingPoint(5.0d))
    assert(InfiniteFloatingPoint(2) + InfiniteFloatingPoint(3) == InfiniteFloatingPoint(5))
  }

  test("Subraction ro.upb.nrs.sl.InfiniteFloatingPoint") {
    assert(InfiniteFloatingPoint(3.0d) - InfiniteFloatingPoint(2.0d) == InfiniteFloatingPoint(1.0d))
    assert(InfiniteFloatingPoint(3) - InfiniteFloatingPoint(2) == InfiniteFloatingPoint(1))
  }

  test("Mul ro.upb.nrs.sl.InfiniteFloatingPoint") {
    assert(InfiniteFloatingPoint(3.0d) * InfiniteFloatingPoint(2.0d) == InfiniteFloatingPoint(6.0d))
    assert(InfiniteFloatingPoint(3) * InfiniteFloatingPoint(2) == InfiniteFloatingPoint(6))
  }

  test("Div ro.upb.nrs.sl.InfiniteFloatingPoint") {
    assert(InfiniteFloatingPoint(6.0d) / InfiniteFloatingPoint(2.0d) == InfiniteFloatingPoint(3.0d))
    assert(InfiniteFloatingPoint(6) / InfiniteFloatingPoint(2) == InfiniteFloatingPoint(3))
  }

  test("Pow ro.upb.nrs.sl.InfiniteFloatingPoint") {
    assert(InfiniteFloatingPoint(2.0d).pow(2) == InfiniteFloatingPoint(4.0d))
    assert(InfiniteFloatingPoint(6).pow(2) == InfiniteFloatingPoint(36))
  }

  test("Nqrt ro.upb.nrs.sl.InfiniteFloatingPoint") {
    assert(InfiniteFloatingPoint(16.0d).nqrt(2) == InfiniteFloatingPoint(4.0d))
    assert(InfiniteFloatingPoint(8).nqrt(3) == InfiniteFloatingPoint(2))
  }

  test("Abs ro.upb.nrs.sl.InfiniteFloatingPoint") {
    assert(InfiniteFloatingPoint(-16.0d).abs== InfiniteFloatingPoint(16.0d))
    assert(InfiniteFloatingPoint(-8).abs == InfiniteFloatingPoint(8))
  }

  test("Signum ro.upb.nrs.sl.InfiniteFloatingPoint") {
    assert(InfiniteFloatingPoint(-16.0d).signum== InfiniteFloatingPoint(-1.0d))
    assert(InfiniteFloatingPoint(8).signum == InfiniteFloatingPoint(1))
  }

  test("Equals ro.upb.nrs.sl.InfiniteFloatingPoint") {
    assert(InfiniteFloatingPoint(16.0d) == InfiniteFloatingPoint(16.0d))
    assert(InfiniteFloatingPoint(16) == InfiniteFloatingPoint(16))
  }

  test("Complex ro.upb.nrs.sl.InfiniteFloatingPoint") {
    val x = InfiniteFloatingPoint(1135984.7834d)
    val y = InfiniteFloatingPoint(3423456.11345d)

    assert(InfiniteFloatingPoint(11) + InfiniteFloatingPoint(34)==InfiniteFloatingPoint(45))

    val sum = x + y
    assert(sum > InfiniteFloatingPoint(4559440.0d))
    val dif = x - y
    assert(dif < InfiniteFloatingPoint(-2287471.0d))
    val mul = InfiniteFloatingPoint(11359.7834d) * InfiniteFloatingPoint(34234.11345d)
    assert(mul > InfiniteFloatingPoint(388892113.0d))
    val div = InfiniteFloatingPoint(11359.7834d) / InfiniteFloatingPoint(34234.11345d)
    assert(div.equals(InfiniteFloatingPoint_NR))
    val pow = InfiniteFloatingPoint(11359.7834d).pow(2)
    assert(pow > InfiniteFloatingPoint(129044678.0d))
    val root = x.nqrt(2)
    assert(root > InfiniteFloatingPoint(1065.0d))
    val nthroot = x.nqrt(3)
    assert(nthroot > InfiniteFloatingPoint(104.0d))
  }


  
  test("Positive Zero and Negative Zero ro.upb.nrs.sl.InfiniteFloatingPoint") {
    val positiveZero = InfiniteFloatingPoint(0.0d)
    val negativeZero = InfiniteFloatingPoint(0.0d) * InfiniteFloatingPoint(-1.0d)
    assert(positiveZero == negativeZero)
    assert( (InfiniteFloatingPoint(-1.0d) + InfiniteFloatingPoint(1.0d)) == negativeZero )
    assert( (InfiniteFloatingPoint(-1.0d) + InfiniteFloatingPoint(1.0d)) == positiveZero )
    assert(positiveZero == (negativeZero * negativeZero) )
    //InfiniteFloatingPoint has only one zero (positive)
    assert(positiveZero.mantisa.value.numerator.sign == negativeZero.mantisa.value.numerator.sign)
  }

}
