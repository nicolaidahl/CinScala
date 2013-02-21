/*
----------------------------------------------------------------------
Copyright (C) 2013 by Nicolai Dahl, Christian Harrington

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
----------------------------------------------------------------------
*/

package dk.itu.cinscala
import CAbstractSyntax._
import CUDAAbstractSyntax._

trait CUDAGenerator extends CGenerator {
  override def generate (prog: Program, varEnv: VarEnv, funEnv: FunEnv): String = {
    val varEnv1 = Set("threadIdx", "threadIdy", "blockIdx", "blockIdy", "blockDim", "gridDim") ++ varEnv
    generateExternalDeclarations(varEnv1, funEnv)(prog.contents)._3
  }
  
  override def generateExternalDeclarations (varEnv: VarEnv, funEnv: FunEnv)(topDecs: List[CExternalDeclaration]): (VarEnv, FunEnv, String) = {
    topDecs match {
      case Nil => (varEnv, funEnv, "")
      case head :: tail =>
        head match {
          case CUDAFunctionDec(functionType, decSpecifiers, declarator, declarationList, compoundStmt) => 
            val functionTypeStr = generateCUDAFunctionQualifier(functionType)
            val (funEnv1, str) = generateFunctionDec(varEnv, funEnv, CFunctionDec(decSpecifiers, declarator, declarationList, compoundStmt))
            val (varEnv1, funEnv2, str1) = generateExternalDeclarations(varEnv, funEnv1)(tail)
            
            (varEnv1, funEnv2, functionTypeStr + " " + str + str1)
          case _ =>
            super.generateExternalDeclarations(varEnv, funEnv)(head :: tail)
        }   
    }
  }
  
  def generateCUDAFunctionQualifier(funcType: CUDAFunctionQualifier): String =
    funcType match {
      case CUDAGlobalQualifier => "__global__"
      case CUDADeviceFuncQualifier => "__device__"
      case CUDAHostQualifier => "__host__"
      case CUDANoInlineQualifier => "__noinline__"
      case CUDAForceInlineQualifier => "__forceinline__"
    }
  
  def generateCUDAVariableQualifier(varType: CUDAVariableQualifier): String =
    varType match {
      case CUDADeviceVarQualifier => "__device__"
      case CUDAConstantQualifier => "__constant__"
      case CUDASharedQualifier => "__shared__"
    }
  
  override def generateExpression(varEnv: VarEnv, funEnv: FunEnv, checkEnv: Boolean = true)(e: CExpression): String =
    e match {
      case CUDAKernelCall(dg, db, postfixExpression, arguments) =>
        val cudaCallStr = "<<<" + dg.toString() + ", " + db.toString() + ">>>"
        generateExpression(varEnv, funEnv)(postfixExpression) + cudaCallStr + arguments.map(generateExpression(varEnv, funEnv)).mkString("(", ", ", ")")
      case CUDAKernelCallExtensive(dg, db, ns, stream, postfixExpression, arguments) =>
        val cudaArgList = List(dg, db, ns.getOrElse(0), stream.getOrElse(0))
        val cudaCallStr = "<<<" + cudaArgList.mkString(", ") + ">>>"
        generateExpression(varEnv, funEnv)(postfixExpression) + cudaCallStr + arguments.map(generateExpression(varEnv, funEnv)).mkString("(", ", ", ")")
      case other => super.generateExpression(varEnv, funEnv, checkEnv)(other)
    }
  
}