import org.scalatest.FunSuite
import ro.upb.nrs.sl._
import ro.upb.nrs.sl.NumberRepresentationSystemConversions._

class NaturalNumber_Tests extends FunSuite {

  numberRepresentationSystemWorkingType = NaturalNumber_NR

  test("Addition 1+2==3") {
    assert(1+NaturalNumber(2)==NaturalNumber(3))
  }

  test("Addition 1+2==3 v2") {
    assert(NaturalNumber(1)+2==NaturalNumber(3))
  }


  test("Subtraction ro.upb.nrs.sl.NaturalNumber") {
    assert(NaturalNumber(3)-2==NaturalNumber(1))
    assert(3 - NaturalNumber(2)==NaturalNumber(1))
  }

  test("Multiplication ro.upb.nrs.sl.NaturalNumber") {
    assert(NaturalNumber(3) * 2==NaturalNumber(6))
    assert(2 * NaturalNumber(3)==NaturalNumber(6))
    assert(NaturalNumber(2) * NaturalNumber(3)==NaturalNumber(6))
  }

  test("Divison ro.upb.nrs.sl.NaturalNumber") {
    assert(NaturalNumber(4) / 2==NaturalNumber(2))
    assert(NaturalNumber(6) / NaturalNumber(2)==NaturalNumber(3))
    assert(6 / NaturalNumber(3)==NaturalNumber(2))
  }

  test("Modulo ro.upb.nrs.sl.NaturalNumber") {
    assert(NaturalNumber(3) % NaturalNumber(2)==NaturalNumber(1))
    assert(NaturalNumber(3) % 2==NaturalNumber(1))
    assert(3 % NaturalNumber(3)==NaturalNumber(0))
  }

  test("Power ro.upb.nrs.sl.NaturalNumber") {
    assert(NaturalNumber(64).pow(NaturalNumber(2))==NaturalNumber(4096))
    assert(NaturalNumber(64).pow(2)==NaturalNumber(4096))
  }

  test("Nth Root ro.upb.nrs.sl.NaturalNumber") {
    //need to double
    assert(NaturalNumber(64).nth_root(2)._1==(NaturalNumber(8), NaturalNumber(0))._1)
    assert(NaturalNumber(64).nth_root(2)._2==(NaturalNumber(8), NaturalNumber(0))._2)
    assert(NaturalNumber(1000).nth_root(3)._1==(NaturalNumber(10), NaturalNumber(0))._1)
    assert(NaturalNumber(1000).nth_root(3)._2==(NaturalNumber(10), NaturalNumber(0))._2)
    assert(NaturalNumber(64).nqrt(2)==NaturalNumber(8))
  }

  test("Abs ro.upb.nrs.sl.NaturalNumber") {
    assert(NaturalNumber(64).abs==NaturalNumber(64))
  }

  test("Signum ro.upb.nrs.sl.NaturalNumber") {
    assert(NaturalNumber(64).signum==NaturalNumber(1))
  }

  test("Complex ro.upb.nrs.sl.NaturalNumber") {
    val x = NaturalNumber(346372547)
    val y = NaturalNumber(274957638)


    val sum = x + y
    assert(sum == NaturalNumber(621330185))
    val dif = x - y
    assert(dif == NaturalNumber(71414909))
    val mul = NaturalNumber(34637) * NaturalNumber(27495)
    assert(mul == NaturalNumber(952344315))
    val div = x / y
    assert(div == NaturalNumber(1))
    assert(NaturalNumber(346372542) / 2 == NaturalNumber(173186271))

    val pow = NaturalNumber(3463).pow(2)
    assert(pow == NaturalNumber(11992369))

    val root = NaturalNumber(71503936).nth_root(2)
    assert(root._1 == (NaturalNumber(8456), NaturalNumber(0))._1)
    assert(root._2 == (NaturalNumber(8456), NaturalNumber(0))._2)

    val nthroot = NaturalNumber(603351125).nth_root(3)
    assert(nthroot._1 == (NaturalNumber(845), NaturalNumber(0))._1)
    assert(nthroot._2 == (NaturalNumber(845), NaturalNumber(0))._2)

  }

  
  test("gcd 4 16 == 4") {
    assert(NaturalNumber(16).gcd(NaturalNumber(4))==NaturalNumber(4))
  }

  test("gcd 4 16 +1 == 5") {
    assert((NaturalNumber(16).gcd(NaturalNumber(4)) + 1)==NaturalNumber(5))
  }

  
  test("2 ** 4 == 16") {
    assert(NaturalNumber(2) ** 4 ==NaturalNumber(16))
  }

  
  test("2 ** 4 == 16 v2") {
    assert(2 ** NaturalNumber(4) ==NaturalNumber(16))
  }
  
  test("SQRT 16") {
    assert(NaturalNumber(16).sqrt==NaturalNumber(4))
  }
  
}