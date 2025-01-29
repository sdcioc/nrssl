package ro.upb.nrs.sl

import scala.annotation.tailrec

object auxiliaryFunctions {
  def minimumEncode(binaryEncode: List[Boolean]): List[Boolean] = {
    @tailrec
    def helper(binaryEncode: List[Boolean]): List[Boolean] = {
      binaryEncode match {
        case Nil => List(false)
        case _ => binaryEncode.last match {
          case true => binaryEncode
          case false => helper(binaryEncode.init)
        }
      }
    }

    binaryEncode match {
      case Nil => Nil
      case _ => helper(binaryEncode)
    }
  }

  def BinaryStringToBooleanString(binaryString: List[Char], acc: List[Boolean]): List[Boolean] = {
    binaryString match {
      case Nil => acc
      case x :: xs => if (x == '1') BinaryStringToBooleanString(xs, true :: acc) else BinaryStringToBooleanString(xs, false :: acc)
    }
  }

  def BinaryEncodeFixedWidth(width: Int, binaryEncode: List[Boolean]): List[Boolean] = {
    width >= binaryEncode.length match {
      case true => binaryEncode match {
        case Nil => List.fill(width)(false)
        case _ => binaryEncode ::: List.fill(width - binaryEncode.length)(false)
      }
      case false => List()
    }
  }

  def BinaryEncodetoBinaryString(binaryEncode: List[Boolean]): String = binaryEncode match {
    case Nil => ""
    case z::zs =>(zs.foldLeft( (if(z) "1" else "0") )( (x, y) => x + (if(y) "1" else "0") )).reverse
  }

  def BinaryStringNegate(binaryString : String) : String = binaryString map (x => if(x=='1') '0' else '1')

  def BinaryEncodetoInt(binaryEncode: List[Boolean]): Int = {
    @tailrec
    def helper(binaryRep: List[Boolean], acc: Int): Int = binaryRep match {
      case Nil => acc
      case y :: ys => if (y) helper(ys, (acc << 1) + 1) else helper(ys, acc << 1)
    }

    helper(binaryEncode.reverse, 0)
  }

}
