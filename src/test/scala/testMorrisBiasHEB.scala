import org.scalatest.FunSuite
import ro.upb.nrs.sl._
import ro.upb.nrs.sl.NumberRepresentationSystemConversions._

class MorrisBiasHEB_Tests extends FunSuite{

  numberRepresentationSystemWorkingType = MorrisBiasHEB_NR(MorrisBiasHEB.default_g_size, MorrisBiasHEB.default_size, MorrisBiasHEB.default_rounding)

  test("toString ro.upb.nrs.sl.MorrisBiasHEB") {
    assert(MorrisBiasHEB(2).toString.equals("2"))
  }

  test("Addition ro.upb.nrs.sl.MorrisBiasHEB") {
    assert(MorrisBiasHEB(2.0d) + MorrisBiasHEB(3.0d) == MorrisBiasHEB(5.0d))
    assert(MorrisBiasHEB(2) + MorrisBiasHEB(3) == MorrisBiasHEB(5))
  }

  test("Subraction ro.upb.nrs.sl.MorrisBiasHEB") {
    assert(MorrisBiasHEB(3.0d) - MorrisBiasHEB(2.0d) == MorrisBiasHEB(1.0d))
    assert(MorrisBiasHEB(3) - MorrisBiasHEB(2) == MorrisBiasHEB(1))
  }

  test("Mul ro.upb.nrs.sl.MorrisBiasHEB") {
    assert(MorrisBiasHEB(3.0d) * MorrisBiasHEB(2.0d) == MorrisBiasHEB(6.0d))
    assert(MorrisBiasHEB(3) * MorrisBiasHEB(2) == MorrisBiasHEB(6))
  }

  test("Div ro.upb.nrs.sl.MorrisBiasHEB") {
    assert(MorrisBiasHEB(6.0d) / MorrisBiasHEB(2.0d) == MorrisBiasHEB(3.0d))
    assert(MorrisBiasHEB(6) / MorrisBiasHEB(2) == MorrisBiasHEB(3))
  }

  test("Pow ro.upb.nrs.sl.MorrisBiasHEB") {
    assert(MorrisBiasHEB(2.0d).pow(2) == MorrisBiasHEB(4.0d))
    assert(MorrisBiasHEB(6).pow(2) == MorrisBiasHEB(36))
  }

  test("Nqrt ro.upb.nrs.sl.MorrisBiasHEB") {
    assert(MorrisBiasHEB(16.0d).nqrt(2) == MorrisBiasHEB(4.0d))
    assert(MorrisBiasHEB(8).nqrt(3) == MorrisBiasHEB(2))
  }

  test("Abs ro.upb.nrs.sl.MorrisBiasHEB") {
    assert(MorrisBiasHEB(-16.0d).abs== MorrisBiasHEB(16.0d))
    assert(MorrisBiasHEB(-8).abs == MorrisBiasHEB(8))
  }

  test("Signum ro.upb.nrs.sl.MorrisBiasHEB") {
    assert(MorrisBiasHEB(-16.0d).signum== MorrisBiasHEB(-1.0d))
    assert(MorrisBiasHEB(8).signum == MorrisBiasHEB(1))
  }

  test("Equals ro.upb.nrs.sl.MorrisBiasHEB") {
    assert(MorrisBiasHEB(16.0d) == MorrisBiasHEB(16.0d))
    assert(MorrisBiasHEB(16) == MorrisBiasHEB(16))
  }

  test("Complex ro.upb.nrs.sl.MorrisBiasHEB") {
    // Asta trebuie fixata ca nici suma cu numere mici nu merge
    val x = MorrisBiasHEB(1135984.7834d)
    val y = MorrisBiasHEB(3423456.11345d)

    assert(MorrisBiasHEB(11) + MorrisBiasHEB(34)==MorrisBiasHEB(45))

    val sum = x + y
    assert(sum >= MorrisBiasHEB(4559440.0d))
    val dif = x - y
    assert(dif == MorrisBiasHEB(-2287471.25d)) //right answer -2287471.33005
    val mul = MorrisBiasHEB(11359.7834d) * MorrisBiasHEB(34234.11345d)
    assert(mul == MorrisBiasHEB(388892096.0d)) //388892113.683
    val div = MorrisBiasHEB(11359.7834d) / MorrisBiasHEB(34234.11345d)
    assert(div >= MorrisBiasHEB(0.331d))
    val pow = MorrisBiasHEB(11359.7834d).pow(2) //
    assert(pow >= MorrisBiasHEB(129044672.895d)) //129044678.895
    val root = x.nqrt(2)
    assert(root >= MorrisBiasHEB(1065.825d))
    val nthroot = x.nqrt(3)
    assert(nthroot >= MorrisBiasHEB(104.341d))
  }


  test("Positive Zero only ro.upb.nrs.sl.MorrisBiasHEB") {
    val positiveZero = MorrisBiasHEB(0.0d)
    val negativeZero = MorrisBiasHEB(0.0d) * MorrisBiasHEB(-1.0d)
    assert(positiveZero == negativeZero)
    assert( (MorrisBiasHEB(-1.0d) + MorrisBiasHEB(1.0d)) == negativeZero )
    assert( (MorrisBiasHEB(-1.0d) + MorrisBiasHEB(1.0d)) == positiveZero )
    assert(positiveZero == (negativeZero * negativeZero) )
    //MorrisBiasHEB has only one zero (positive)
    assert(positiveZero.value.sign == negativeZero.value.sign)
  }

  
}
