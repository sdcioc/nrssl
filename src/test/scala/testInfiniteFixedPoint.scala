import org.scalatest.FunSuite
import ro.upb.nrs.sl._
import ro.upb.nrs.sl.NumberRepresentationSystemConversions._

class InfiniteFixedPoint_Tests extends FunSuite{

  numberRepresentationSystemWorkingType = InfiniteFixedPoint_NR

  test("toString ro.upb.nrs.sl.InfiniteFixedPoint") {
    // pare ca orice as printa cu toString da 0, iar cu toInternalString : 0 bitLength: 0 binary:0/2^16
    assert(InfiniteFixedPoint(2).toString.equals("2"))
  }

  test("Addition ro.upb.nrs.sl.InfiniteFixedPoint") {
    assert(InfiniteFixedPoint(2.0d) + InfiniteFixedPoint(3.0d) == InfiniteFixedPoint(5.0d))
    assert(InfiniteFixedPoint(2) + InfiniteFixedPoint(3) == InfiniteFixedPoint(5))
  }

  test("Subraction ro.upb.nrs.sl.InfiniteFixedPoint") {
    assert(InfiniteFixedPoint(3.0d) - InfiniteFixedPoint(2.0d) == InfiniteFixedPoint(1.0d))
    assert(InfiniteFixedPoint(3) - InfiniteFixedPoint(2) == InfiniteFixedPoint(1))
  }

  test("Mul ro.upb.nrs.sl.InfiniteFixedPoint") {
    assert(InfiniteFixedPoint(3.0d) * InfiniteFixedPoint(2.0d) == InfiniteFixedPoint(6.0d))
    assert(InfiniteFixedPoint(3) * InfiniteFixedPoint(2) == InfiniteFixedPoint(6))
  }

  test("Div ro.upb.nrs.sl.InfiniteFixedPoint") {
    assert(InfiniteFixedPoint(6.0d) / InfiniteFixedPoint(2.0d) == InfiniteFixedPoint(3.0d))
    assert(InfiniteFixedPoint(6) / InfiniteFixedPoint(2) == InfiniteFixedPoint(3))
    //rezultatele trebuie sa fie reprzentabile in ro.upb.nrs.sl.InfiniteFixedPoint DA
    //assert(ro.upb.nrs.sl.InfiniteFixedPoint(3.6d) / ro.upb.nrs.sl.InfiniteFixedPoint(1.5d) == ro.upb.nrs.sl.InfiniteFixedPoint(2.4d))
    assert(InfiniteFixedPoint(3.0d) / InfiniteFixedPoint(1.5d) == InfiniteFixedPoint(2.0d))
  }

  test("Pow ro.upb.nrs.sl.InfiniteFixedPoint") {
    assert(InfiniteFixedPoint(2.0d).pow(2) == InfiniteFixedPoint(4.0d))
    assert(InfiniteFixedPoint(6).pow(2) == InfiniteFixedPoint(36))
    val temp = InfiniteFixedPoint(2.0d).pow(InfiniteFixedPoint(2.25d))
    assert(temp > InfiniteFixedPoint(4.75d) && temp < InfiniteFixedPoint(4.76d))
  }

//  test("NthRoot ro.upb.nrs.sl.InfiniteFixedPoint") {
//    assert(ro.upb.nrs.sl.InfiniteFixedPoint(16.0d).nqrt(2) == ro.upb.nrs.sl.InfiniteFixedPoint(4.0d))
//    assert(ro.upb.nrs.sl.InfiniteFixedPoint(8).nqrt(3) == ro.upb.nrs.sl.InfiniteFixedPoint(2))
//  }

  test("Abs ro.upb.nrs.sl.InfiniteFixedPoint") {
    assert(InfiniteFixedPoint(-16.0d).abs== InfiniteFixedPoint(16.0d))
    assert(InfiniteFixedPoint(-8).abs == InfiniteFixedPoint(8))
  }

  test("Signum ro.upb.nrs.sl.InfiniteFixedPoint") {
    assert(InfiniteFixedPoint(-16.0d).signum== InfiniteFixedPoint(-1.0d))
    assert(InfiniteFixedPoint(8).signum == InfiniteFixedPoint(1))
  }

  test("Equals ro.upb.nrs.sl.InfiniteFixedPoint") {
    assert(InfiniteFixedPoint(16.0d) == InfiniteFixedPoint(16.0d))
    assert(InfiniteFixedPoint(16) == InfiniteFixedPoint(16))
  }

  test("Complex ro.upb.nrs.sl.InfiniteFixedPoint") {
    val x = InfiniteFixedPoint(1135984.7834d)
    val y = InfiniteFixedPoint(3423456.11345d)


    val sum = x + y
    assert(sum > InfiniteFixedPoint(4559440.0d))
    val dif = x - y
    assert(dif < InfiniteFixedPoint(-2287471.0d))
    val mul = InfiniteFixedPoint(11359.7834d) * InfiniteFixedPoint(34234.11345d)
    assert(mul > InfiniteFixedPoint(388892113.0d))
    val div = InfiniteFixedPoint(11359.7834d) / InfiniteFixedPoint(34234.11345d)
    assert(div.equals(InfiniteFixedPoint_NR)) // NR in ifinite precision float
    val pow = InfiniteFixedPoint(11359.7834d).pow(2)
    assert(pow > InfiniteFixedPoint(129044678.0d))
//    val root = x.nqrt(2)
//    assert(root > ro.upb.nrs.sl.InfiniteFixedPoint(1065.0d))
//    val nthroot = x.nqrt(3)
//    assert(nthroot > ro.upb.nrs.sl.InfiniteFixedPoint(104.0d))
  }

}
