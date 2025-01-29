package ro.upb.nrs.sl

trait AccumulatorTrait {
    def fusedMultiply(that : AccumulatorTrait, size : Int, fractionSize : Int) : FixedPoint_B 
}

class Accumulator(value_c : FixedPoint_B, accumulatorSize : Int, accumulatorFractionSize : Int) {
    val value : FixedPoint_B = value_c

    def clear() : Accumulator = new Accumulator(
        FixedPoint(IntegerNumber(0), accumulatorSize, accumulatorFractionSize, NoRounding),
        accumulatorSize,
        accumulatorFractionSize
    )

    def +(that : Accumulator) : Accumulator = new Accumulator(this.value + that.value, accumulatorSize, accumulatorFractionSize)
    def -(that : Accumulator) : Accumulator = new Accumulator(this.value - that.value, accumulatorSize, accumulatorFractionSize)

    def fma(operand1 : AccumulatorTrait, operand2 : AccumulatorTrait) : Accumulator = new Accumulator(
        this.value + operand1.fusedMultiply(operand2, accumulatorSize, accumulatorFractionSize),
        accumulatorSize,
        accumulatorFractionSize
    )

    def toRationalNumber : RationalNumber_B = this.value.toRationalNumber
}