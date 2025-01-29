import org.scalatest.FunSuite
import ro.upb.nrs.sl._
import ro.upb.nrs.sl.NumberRepresentationSystemConversions._

class MorrisUnaryHEB_Tests extends FunSuite{

  numberRepresentationSystemWorkingType = MorrisUnaryHEB_NR(MorrisUnaryHEB.default_size, MorrisUnaryHEB.default_rounding)

  test("toString ro.upb.nrs.sl.MorrisUnaryHEB") {
    assert(MorrisUnaryHEB(2).toString.equals("2"))
  }

  test("Addition ro.upb.nrs.sl.MorrisUnaryHEB") {
    assert(MorrisUnaryHEB(2.0d) + MorrisUnaryHEB(3.0d) == MorrisUnaryHEB(5.0d))
    assert(MorrisUnaryHEB(2) + MorrisUnaryHEB(3) == MorrisUnaryHEB(5))
  }

  test("Subraction ro.upb.nrs.sl.MorrisUnaryHEB") {
    assert(MorrisUnaryHEB(3.0d) - MorrisUnaryHEB(2.0d) == MorrisUnaryHEB(1.0d))
    assert(MorrisUnaryHEB(3) - MorrisUnaryHEB(2) == MorrisUnaryHEB(1))
  }

  test("Mul ro.upb.nrs.sl.MorrisUnaryHEB") {
    assert(MorrisUnaryHEB(3.0d) * MorrisUnaryHEB(2.0d) == MorrisUnaryHEB(6.0d))
    assert(MorrisUnaryHEB(3) * MorrisUnaryHEB(2) == MorrisUnaryHEB(6))
  }

  test("Div ro.upb.nrs.sl.MorrisUnaryHEB") {
    assert(MorrisUnaryHEB(6.0d) / MorrisUnaryHEB(2.0d) == MorrisUnaryHEB(3.0d))
    assert(MorrisUnaryHEB(6) / MorrisUnaryHEB(2) == MorrisUnaryHEB(3))
  }

  test("Pow ro.upb.nrs.sl.MorrisUnaryHEB") {
    assert(MorrisUnaryHEB(2.0d).pow(2) == MorrisUnaryHEB(4.0d))
    assert(MorrisUnaryHEB(6).pow(2) == MorrisUnaryHEB(36))
  }

  test("Nqrt ro.upb.nrs.sl.MorrisUnaryHEB") {
    assert(MorrisUnaryHEB(16.0d).nqrt(2) == MorrisUnaryHEB(4.0d))
    assert(MorrisUnaryHEB(8).nqrt(3) == MorrisUnaryHEB(2))
  }

  test("Abs ro.upb.nrs.sl.MorrisUnaryHEB") {
    assert(MorrisUnaryHEB(-16.0d).abs== MorrisUnaryHEB(16.0d))
    assert(MorrisUnaryHEB(-8).abs == MorrisUnaryHEB(8))
  }

  test("Signum ro.upb.nrs.sl.MorrisUnaryHEB") {
    assert(MorrisUnaryHEB(-16.0d).signum== MorrisUnaryHEB(-1.0d))
    assert(MorrisUnaryHEB(8).signum == MorrisUnaryHEB(1))
  }

  test("Equals ro.upb.nrs.sl.MorrisUnaryHEB") {
    assert(MorrisUnaryHEB(16.0d) == MorrisUnaryHEB(16.0d))
    assert(MorrisUnaryHEB(16) == MorrisUnaryHEB(16))
  }

  test("Complex ro.upb.nrs.sl.MorrisUnaryHEB") {
    // Asta trebuie fixata ca nici suma cu numere mici nu merge
    val x = MorrisUnaryHEB(1135984.7834d)
    val y = MorrisUnaryHEB(3423456.11345d)

    assert(MorrisUnaryHEB(11) + MorrisUnaryHEB(34)==MorrisUnaryHEB(45))

    val sum = x + y
    assert(sum >= MorrisUnaryHEB(4559440.0d))
    val dif = x - y
    assert(dif == MorrisUnaryHEB(-2287471.25d)) //right answer -2287471.33005
    val mul = MorrisUnaryHEB(11359.7834d) * MorrisUnaryHEB(34234.11345d)
    assert(mul == MorrisUnaryHEB(388892096.0d)) //388892113.683
    val div = MorrisUnaryHEB(11359.7834d) / MorrisUnaryHEB(34234.11345d)
    assert(div >= MorrisUnaryHEB(0.331d))
    val pow = MorrisUnaryHEB(11359.7834d).pow(2) //
    assert(pow >= MorrisUnaryHEB(129044600.0d)) //129044678.895
    val root = x.nqrt(2)
    assert(root >= MorrisUnaryHEB(1065.825d))
    val nthroot = x.nqrt(3)
    assert(nthroot >= MorrisUnaryHEB(104.341d))
  }


  test("Positive Zero only ro.upb.nrs.sl.MorrisUnaryHEB") {
    val positiveZero = MorrisUnaryHEB(0.0d)
    val negativeZero = MorrisUnaryHEB(0.0d) * MorrisUnaryHEB(-1.0d)
    assert(positiveZero == negativeZero)
    assert( (MorrisUnaryHEB(-1.0d) + MorrisUnaryHEB(1.0d)) == negativeZero )
    assert( (MorrisUnaryHEB(-1.0d) + MorrisUnaryHEB(1.0d)) == positiveZero )
    assert(positiveZero == (negativeZero * negativeZero) )
    //MorrisUnaryHEB has only one zero (positive)
    assert(positiveZero.value.sign == negativeZero.value.sign)
  }

  
}
