import org.scalatest.FunSuite
import ro.upb.nrs.sl._
import ro.upb.nrs.sl.NumberRepresentationSystemConversions._

class IntegerNumber_Tests extends FunSuite {

  numberRepresentationSystemWorkingType = IntegerNumber_NR


  test("Addition 1+2==3") {
    assert(1+IntegerNumber(2)==IntegerNumber(3))
  }

  test("Addition 1+2==3 v2") {
    assert(IntegerNumber(1)+2==IntegerNumber(3))
  }

  test("Subtraction ro.upb.nrs.sl.IntegerNumber") {
    assert(IntegerNumber(3)-2==IntegerNumber(1))
    assert(IntegerNumber(2)-3==IntegerNumber(-1))
    assert(3 - IntegerNumber(2)==IntegerNumber(1))
    assert(2 - IntegerNumber(3)==IntegerNumber(-1))
    assert(IntegerNumber(3) - IntegerNumber(2)==IntegerNumber(1))
    assert(IntegerNumber(2) - IntegerNumber(3)==IntegerNumber(-1))
  }

  test("Multiplication ro.upb.nrs.sl.IntegerNumber") {
    assert(IntegerNumber(3) * 2==IntegerNumber(6))
    assert(2 * IntegerNumber(3)==IntegerNumber(6))
    assert(IntegerNumber(-2) * IntegerNumber(3)==IntegerNumber(-6))
  }

  test("Divison ro.upb.nrs.sl.IntegerNumber") {
    assert(IntegerNumber(4) / 2==IntegerNumber(2))
    assert(IntegerNumber(6) / IntegerNumber(2)==IntegerNumber(3))
    assert(-6 / IntegerNumber(3)==IntegerNumber(-2))
  }

  test("Modulo ro.upb.nrs.sl.IntegerNumber") {
    assert(IntegerNumber(3) % IntegerNumber(2)==IntegerNumber(1))
    assert(IntegerNumber(-3) % IntegerNumber(2)==IntegerNumber(1))
    assert(IntegerNumber(3) % 2==IntegerNumber(1))
    assert(3 % IntegerNumber(2)==IntegerNumber(1))
  }

  test("Power ro.upb.nrs.sl.IntegerNumber") {
    assert(IntegerNumber(64).pow(IntegerNumber(2))==IntegerNumber(4096))
    assert(IntegerNumber(64).pow(2)==IntegerNumber(4096))
    assert(IntegerNumber(-10).pow(2)==IntegerNumber(100))
    assert(IntegerNumber(-10).pow(IntegerNumber(3))==IntegerNumber(-1000))
    assert(IntegerNumber(-10).pow(3)==IntegerNumber(-1000))
  }

  test("Nth Root ro.upb.nrs.sl.IntegerNumber") {
    // ._2 is ro.upb.nrs.sl.NaturalNumber_B
    assert(IntegerNumber(64).nth_root(2)._1==(IntegerNumber(8), NaturalNumber(0))._1)
    assert(IntegerNumber(64).nth_root(2)._2==(IntegerNumber(8), NaturalNumber(0))._2)
    assert(IntegerNumber(1000).nth_root(3)._1==(IntegerNumber(10), NaturalNumber(0))._1)
    assert(IntegerNumber(1000).nth_root(3)._2==(IntegerNumber(10), NaturalNumber(0))._2)
    assert(IntegerNumber(64).nqrt(2)==IntegerNumber(8))
  }

  test("Abs ro.upb.nrs.sl.IntegerNumber") {
    assert(IntegerNumber(64).abs==IntegerNumber(64))
    assert(IntegerNumber(-64).abs==IntegerNumber(64))
  }

  test("Signum ro.upb.nrs.sl.IntegerNumber") {
    assert(IntegerNumber(64).signum==IntegerNumber(1))
    assert(IntegerNumber(-64).signum==IntegerNumber(-1))
  }

  test("Equals ro.upb.nrs.sl.IntegerNumber") {
    assert(IntegerNumber(8)==IntegerNumber(8))
    assert(IntegerNumber(-8)==IntegerNumber(-8))
  }

  test("Complex ro.upb.nrs.sl.IntegerNumber") {
    val x = IntegerNumber(346372547)
    val y = IntegerNumber(274957638)


    val sum = x + y
    assert(sum == IntegerNumber(621330185))
    val dif = x - y
    assert(dif == IntegerNumber(71414909))
    val mul = IntegerNumber(34637) * IntegerNumber(27495)
    assert(mul == IntegerNumber(952344315))
    assert(IntegerNumber(-3360) * IntegerNumber(34345) == IntegerNumber(-115399200))
    val div = x / y
    assert(div == IntegerNumber(1))
    assert(IntegerNumber(346372542) / 2 == IntegerNumber(173186271))
    assert(IntegerNumber(346372542) / -2 == IntegerNumber(-173186271))
    assert(IntegerNumber(336065825) / IntegerNumber(34345) == IntegerNumber(9785))

    // Cred ca e busit power ca se duce destul de repede spre 0 desi il declar cu size 64
    val pow = IntegerNumber(3463).pow(2)
    assert(pow == IntegerNumber(11992369))

    val root = IntegerNumber(71503936).nth_root(2)
    assert(root._1 == (IntegerNumber(8456), NaturalNumber(0))._1)
    assert(root._2 == (IntegerNumber(8456), NaturalNumber(0))._2)

    val nthroot = IntegerNumber(603351125).nth_root(3)
    assert(nthroot._1 == (IntegerNumber(845), NaturalNumber(0))._1)
    assert(nthroot._2 == (IntegerNumber(845), NaturalNumber(0))._2)

  }

  test("SQRT 16") {
    assert(IntegerNumber(16).sqrt==IntegerNumber(4))
  }
}