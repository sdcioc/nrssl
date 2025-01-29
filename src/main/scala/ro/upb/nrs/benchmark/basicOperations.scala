package ro.upb.nrs.benchmark


import java.io.PrintWriter
import java.io.File

import scala.annotation.tailrec
import ro.upb.nrs.sl._
import ro.upb.nrs.dsp._

object arithmeticOperations {
    def operation(nrs : FixedPrecisionNumberRepresentationSystem, operationType : String) = {
        val size = basicBenchmarkFunctions.getNRS_size(nrs)
        val maximum : Long = (BigInt(1).toLong << size)
        var index : Long = 0;
        var jndex : Long = 0;
        val filePath = "results/" + (
            if(operationType == "ADD") "addition"
            else if(operationType == "MUL") "multiplication"
            else if(operationType == "SUB") "subtraction"
            else if(operationType == "DIV") "division"
            else if(operationType == "POW") "power"
            else throw new NotImplementedError("ro.upb.nrs.benchmark.arithmeticOperations operation inexistent")
            ) + "/" + basicBenchmarkFunctions.toStringFromNRS(nrs) + ".txt"
        val fileAccuracy = new File(filePath)
        val filePrinterAccuracy = new PrintWriter(fileAccuracy)
        var totalTime = 0.0d
        while(index < maximum) {
            jndex = 0;
            val binary1 = auxiliaryFunctions.BinaryEncodetoBinaryString(auxiliaryFunctions.BinaryEncodeFixedWidth(size, NaturalNumber(index).binaryEncode))
            val value1_nrs = basicBenchmarkFunctions.fromBinaryString(binary1, nrs)
            val value1_q = value1_nrs.toRationalNumber
            while(jndex < maximum) {
                val binary2 = auxiliaryFunctions.BinaryEncodetoBinaryString(auxiliaryFunctions.BinaryEncodeFixedWidth(size, NaturalNumber(jndex).binaryEncode))
                val value2_nrs = basicBenchmarkFunctions.fromBinaryString(binary2, nrs)
                val value2_q = value2_nrs.toRationalNumber
                val beforeTime = System.nanoTime
                val result_nrs = (if(operationType == "ADD") value1_nrs + value2_nrs
                    else if(operationType == "MUL") value1_nrs * value2_nrs
                    else if(operationType == "SUB") value1_nrs - value2_nrs
                    else if(operationType == "DIV") value1_nrs / value2_nrs
                    else if(operationType == "POW") value1_nrs.pow(value2_nrs)
                    else throw new NotImplementedError("ro.upb.nrs.benchmark.arithmeticOperations operation inexistent")
                )
                val afterTime = System.nanoTime
                totalTime = totalTime + (afterTime-beforeTime) / 1e9d
                val result_q = (if(operationType == "ADD") value1_q + value2_q
                    else if(operationType == "MUL") value1_q * value2_q
                    else if(operationType == "SUB") value1_q - value2_q
                    else if(operationType == "DIV") value1_q / value2_q
                    else if(operationType == "POW") value1_q.pow(value2_q)
                    else throw new NotImplementedError("ro.upb.nrs.benchmark.arithmeticOperations operation inexistent")
                )
                filePrinterAccuracy.write(binary1 + " " + binary2 + " " + value1_nrs + " " + value2_nrs + " " + result_nrs.toRationalNumber + " " + result_q + " " + (result_nrs.toRationalNumber/result_q) + " " + (result_q-result_nrs.toRationalNumber) + "\n")
                //println( "i: " + index + "j: " + jndex )
                jndex = jndex + 1
            }
            index = index + 1
        }
        filePrinterAccuracy.write("Time:" + totalTime)
        filePrinterAccuracy.close()
    }
    
    def testOperation(nrs : FixedPrecisionNumberRepresentationSystem, operationType : String) = {
        val size = basicBenchmarkFunctions.getNRS_size(nrs)
        val maximum : Long = (BigInt(1).toLong << size)
        var index : Long = 0;
        var jndex : Long = 0;
        while(index < maximum) {
            jndex = 0;
            val binary1 = auxiliaryFunctions.BinaryEncodetoBinaryString(auxiliaryFunctions.BinaryEncodeFixedWidth(size, NaturalNumber(index).binaryEncode))
            val value1_nrs = basicBenchmarkFunctions.fromBinaryString(binary1, nrs)
            val value1_q = value1_nrs.toRationalNumber
            while(jndex < maximum) {
                val binary2 = auxiliaryFunctions.BinaryEncodetoBinaryString(auxiliaryFunctions.BinaryEncodeFixedWidth(size, NaturalNumber(jndex).binaryEncode))
                val value2_nrs = basicBenchmarkFunctions.fromBinaryString(binary2, nrs)
                val value2_q = value2_nrs.toRationalNumber
                val result_nrs = (if(operationType == "ADD") value1_nrs + value2_nrs
                    else if(operationType == "MUL") value1_nrs * value2_nrs
                    else if(operationType == "SUB") value1_nrs - value2_nrs
                    else if(operationType == "DIV") value1_nrs / value2_nrs
                    else if(operationType == "POW") value1_nrs.pow(value2_nrs)
                    else throw new NotImplementedError("ro.upb.nrs.benchmark.arithmeticOperations operation inexistent")
                )
                val result_q = (if(operationType == "ADD") value1_q + value2_q
                    else if(operationType == "MUL") value1_q * value2_q
                    else if(operationType == "SUB") value1_q - value2_q
                    else if(operationType == "DIV") value1_q / value2_q
                    else if(operationType == "POW") value1_q.pow(value2_q)
                    else throw new NotImplementedError("ro.upb.nrs.benchmark.arithmeticOperations operation inexistent")
                )
                println(binary1 + " " + binary2 + " " + value1_nrs + " " + value2_nrs + " " + result_nrs.toRationalNumber + " " + result_q + " " + (result_nrs.toRationalNumber/result_q) + " " + (result_q-result_nrs.toRationalNumber) + "\n")
                //println( "i: " + index + "j: " + jndex )
                jndex = jndex + 1
            }
            index = index + 1
        }
    }
    def addition(nrs : FixedPrecisionNumberRepresentationSystem) = {
        this.operation(nrs, "ADD")
    }

    def testOperations = {
        val size = 12
        val nrssList = List(
            Posit(0.0d, 2, size, RoundEven),
            MorrisUnaryHEB(1.0d, size, RoundEven),
            MorrisBiasHEB(1.0d, 3, size, RoundEven),
            Morris(1.0d, 3, size, RoundZero),
            MorrisHEB(1.0d, 3, size, RoundZero),
            FixedPoint(1.0d, size, 6, RoundEven),
            IEEE754(0.0d, 3, 4, RoundEven),
            FixedFloatingPoint(0.0d, 4, 7, RoundEven)
        )
        //val nrssList = List(IEEE754(0.0d, 3, 4, RoundEven), FixedFloatingPoint(0.0d, 4, 7, RoundEven))
        val operationList = List("ADD", "MUL", "DIV")//, "POW")
        //val operationList = List("POW")
        var threadList = for ( nrs <- nrssList; operationType <- operationList) yield (new arithmeticOperationBenchmarkThread(nrs, operationType))
        for(thread <- threadList) {
            thread.start()
        }
        for(thread <- threadList) {
            thread.join()
        }
    }

    
    def testPOWOperation = {
        val size = 8
        val nrssList = List(
            Posit(0.0d, 2, size, RoundEven),
            MorrisUnaryHEB(1.0d, size, RoundEven),
            MorrisBiasHEB(1.0d, 2, size, RoundEven),
            Morris(1.0d, 2, size, RoundZero),
            MorrisHEB(1.0d, 2, size, RoundZero),
            FixedPoint(1.0d, size, 4, RoundEven),
            IEEE754(0.0d, 3, 4, RoundEven),
            FixedFloatingPoint(0.0d, 3, 4, RoundEven)
        )
        val operationList = List("POW")
        var threadList = for ( nrs <- nrssList; operationType <- operationList) yield (new arithmeticOperationBenchmarkThread(nrs, operationType))
        for(thread <- threadList) {
            thread.start()
        }
        for(thread <- threadList) {
            thread.join()
        }
    }

    def singleOperation(nrs : FixedPrecisionNumberRepresentationSystem, operationType : String) = {
        val size = basicBenchmarkFunctions.getNRS_size(nrs)
        val maximum : Long = (BigInt(1).toLong << size)
        var index : Long = 0;
        var jndex : Long = 0;
        val filePath = "results/" + (
            if(operationType == "SQRT") "square_root"
            else if(operationType == "LN") "natural_logarithm"
            else if(operationType == "INV") "inverse"
            else if(operationType == "EXP") "exponential"
            else if(operationType == "SIN") "sinus"
            else if(operationType == "SINH") "hyperbolic_sinus"
            else if(operationType == "CRT") "cube_root"
            else throw new NotImplementedError("ro.upb.nrs.benchmark.arithmeticOperations operation inexistent")
            ) + "/" + basicBenchmarkFunctions.toStringFromNRS(nrs) + ".txt"
        val fileAccuracy = new File(filePath)
        val filePrinterAccuracy = new PrintWriter(fileAccuracy)
        var totalTime = 0.0d
        while(index < maximum) {
            jndex = 0;
            val binary1 = auxiliaryFunctions.BinaryEncodetoBinaryString(auxiliaryFunctions.BinaryEncodeFixedWidth(size, NaturalNumber(index).binaryEncode))
            val value1_nrs = basicBenchmarkFunctions.fromBinaryString(binary1, nrs)
            val value1_q = value1_nrs.toRationalNumber
            val beforeTime = System.nanoTime
            val result_nrs = (if(operationType == "SQRT") value1_nrs.sqrt
                            else if(operationType == "LN") value1_nrs.ln
                            else if(operationType == "INV") value1_nrs.inverse
                            else if(operationType == "EXP") value1_nrs.exp
                            else if(operationType == "SIN") value1_nrs.sin
                            else if(operationType == "SINH") value1_nrs.sinh
                            else if(operationType == "CRT") value1_nrs.nqrt(3)
                            else throw new NotImplementedError("ro.upb.nrs.benchmark.arithmeticOperations operation inexistent")
                            )
            val afterTime = System.nanoTime
            totalTime = totalTime + (afterTime-beforeTime) / 1e9d
            val result_q = (if(operationType == "SQRT") value1_q.sqrt
                            else if(operationType == "LN") value1_q.ln
                            else if(operationType == "INV") value1_q.inverse
                            else if(operationType == "EXP") value1_q.exp
                            else if(operationType == "SIN") value1_q.sin
                            else if(operationType == "SINH") value1_q.sinh
                            else if(operationType == "CRT") value1_q.nqrt(3)
                            else throw new NotImplementedError("ro.upb.nrs.benchmark.arithmeticOperations operation inexistent")
                         )
            filePrinterAccuracy.write(binary1 + " " + value1_nrs + " " + result_nrs.toRationalNumber + " " + result_q + " " + (result_nrs.toRationalNumber/result_q) + " " + (result_q-result_nrs.toRationalNumber) + "\n")
            index = index + 1
        }
        filePrinterAccuracy.write("Time:" + totalTime)
        filePrinterAccuracy.close()
    }
    def testSingleOperations = {
        val size = 16
        val nrssList = List(
            Posit(0.0d, 2, size, RoundEven),
            MorrisUnaryHEB(1.0d, size, RoundEven),
            MorrisBiasHEB(1.0d, 4, size, RoundEven),
            Morris(1.0d, 4, size, RoundZero),
            MorrisHEB(1.0d, 4, size, RoundZero),
            FixedPoint(1.0d, size, 8, RoundEven)
        )
        val operationList = List("SQRT", "LN", "INV", "EXP", "SIN", "SINH", "CRT")
        var threadList = for ( nrs <- nrssList; operationType <- operationList) yield (new arithmeticSingleOperationBenchmarkThread(nrs, operationType))
        for(thread <- threadList) {
            thread.start()
        }
        for(thread <- threadList) {
            thread.join()
        }
    }
    def testSingleOperations2 = {
        val size = 16
        val nrssList = List(IEEE754(0.0d, 5, 10, RoundEven), FixedFloatingPoint(0.0d, 5, 10, RoundEven))
        //val nrssList = List(Posit(0.0d, 2, size, RoundEven), MorrisUnaryHEB(1.0d, size, RoundEven), MorrisBiasHEB(1.0d, 4, size, RoundEven), Morris(1.0d, 4, size, RoundZero), MorrisHEB(1.0d, 4, size, RoundZero), FixedPoint(1.0d, size, 8, RoundEven))
        val operationList = List("SQRT", "LN", "INV", "EXP", "SIN", "SINH", "CRT")
        var threadList = for ( nrs <- nrssList; operationType <- operationList) yield (new arithmeticSingleOperationBenchmarkThread(nrs, operationType))
        for(thread <- threadList) {
            thread.start()
        }
        for(thread <- threadList) {
            thread.join()
        }
    }
}

class arithmeticOperationBenchmarkThread(nrs : FixedPrecisionNumberRepresentationSystem, operationType : String) extends Thread {
    override def run(): Unit = {
        arithmeticOperations.operation(nrs, operationType)
    }
}

class arithmeticSingleOperationBenchmarkThread(nrs : FixedPrecisionNumberRepresentationSystem, operationType : String) extends Thread {
    override def run(): Unit = {
        arithmeticOperations.singleOperation(nrs, operationType)
    }
}