import org.scalatest.FunSuite
import ro.upb.nrs.sl._
import ro.upb.nrs.sl.NumberRepresentationSystemConversions._

class MorrisHEB_Tests extends FunSuite{

  numberRepresentationSystemWorkingType = MorrisHEB_NR(MorrisHEB.default_g_size, MorrisHEB.default_size, MorrisHEB.default_rounding)

  test("toString ro.upb.nrs.sl.MorrisHEB") {
    assert(MorrisHEB(2).toString.equals("2"))
  }

  test("Addition ro.upb.nrs.sl.MorrisHEB") {
    assert(MorrisHEB(2.0d) + MorrisHEB(3.0d) == MorrisHEB(5.0d))
    assert(MorrisHEB(2) + MorrisHEB(3) == MorrisHEB(5))
  }

  test("Subraction ro.upb.nrs.sl.MorrisHEB") {
    assert(MorrisHEB(3.0d) - MorrisHEB(2.0d) == MorrisHEB(1.0d))
    assert(MorrisHEB(3) - MorrisHEB(2) == MorrisHEB(1))
  }

  test("Mul ro.upb.nrs.sl.MorrisHEB") {
    assert(MorrisHEB(3.0d) * MorrisHEB(2.0d) == MorrisHEB(6.0d))
    assert(MorrisHEB(3) * MorrisHEB(2) == MorrisHEB(6))
  }

  test("Div ro.upb.nrs.sl.MorrisHEB") {
    assert(MorrisHEB(6.0d) / MorrisHEB(2.0d) == MorrisHEB(3.0d))
    assert(MorrisHEB(6) / MorrisHEB(2) == MorrisHEB(3))
  }

  test("Pow ro.upb.nrs.sl.MorrisHEB") {
    assert(MorrisHEB(2.0d).pow(2) == MorrisHEB(4.0d))
    assert(MorrisHEB(6).pow(2) == MorrisHEB(36))
  }

  test("Nqrt ro.upb.nrs.sl.MorrisHEB") {
    assert(MorrisHEB(16.0d).nqrt(2) == MorrisHEB(4.0d))
    assert(MorrisHEB(8).nqrt(3) == MorrisHEB(2))
  }

  test("Abs ro.upb.nrs.sl.MorrisHEB") {
    assert(MorrisHEB(-16.0d).abs== MorrisHEB(16.0d))
    assert(MorrisHEB(-8).abs == MorrisHEB(8))
  }

  test("Signum ro.upb.nrs.sl.MorrisHEB") {
    assert(MorrisHEB(-16.0d).signum== MorrisHEB(-1.0d))
    assert(MorrisHEB(8).signum == MorrisHEB(1))
  }

  test("Equals ro.upb.nrs.sl.MorrisHEB") {
    assert(MorrisHEB(16.0d) == MorrisHEB(16.0d))
    assert(MorrisHEB(16) == MorrisHEB(16))
  }

  test("Complex ro.upb.nrs.sl.MorrisHEB") {
    // Asta trebuie fixata ca nici suma cu numere mici nu merge
    val x = MorrisHEB(1135984.7834d)
    val y = MorrisHEB(3423456.11345d)

    assert(MorrisHEB(11) + MorrisHEB(34)==MorrisHEB(45))

    val sum = x + y
    assert(sum >= MorrisHEB(4559440.0d))
    val dif = x - y
    assert(dif == MorrisHEB(-2287471.0d))
    val mul = MorrisHEB(11359.7834d) * MorrisHEB(34234.11345d)
    assert(mul == MorrisHEB(388892113.0d))
    val div = MorrisHEB(11359.7834d) / MorrisHEB(34234.11345d)
    assert(div >= MorrisHEB(0.331d))
    val pow = MorrisHEB(11359.7834d).pow(2)
    assert(pow >= MorrisHEB(129044678.0d))
    val root = x.nqrt(2)
    assert(root >= MorrisHEB(1065.825d))
    val nthroot = x.nqrt(3)
    assert(nthroot >= MorrisHEB(104.341d))
  }


  test("Positive Zero only ro.upb.nrs.sl.MorrisHEB") {
    val positiveZero = MorrisHEB(0.0d)
    val negativeZero = MorrisHEB(0.0d) * MorrisHEB(-1.0d)
    assert(positiveZero == negativeZero)
    assert( (MorrisHEB(-1.0d) + MorrisHEB(1.0d)) == negativeZero )
    assert( (MorrisHEB(-1.0d) + MorrisHEB(1.0d)) == positiveZero )
    assert(positiveZero == (negativeZero * negativeZero) )
    //MorrisHEB has only one zero (positive)
    assert(positiveZero.value.sign == negativeZero.value.sign)
  }

  
}
