package dk.itu.c
import CAbstractSyntax._
import CUDAAbstractSyntax._


trait CUDAGenerator extends CGenerator {
  
  override def generate (prog: Program, varEnv: VarEnv, funEnv: FunEnv): String = {
    generateExternalDeclarations(varEnv, funEnv)(prog.contents)._3
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
  
  override def generateExpression(varEnv: VarEnv, funEnv: FunEnv)(e: CExpression): String =
    e match {
      case CUDAKernelCall(dg, db, postfixExpression, arguments) =>
        val cudaCallStr = "<<<" + dg.toString() + ", " + db.toString() + ">>>"
        generateExpression(varEnv, funEnv)(postfixExpression) + cudaCallStr + arguments.map(generateExpression(varEnv, funEnv)).mkString("(", ", ", ")")
      case CUDAKernelCallExtensive(dg, db, ns, stream, postfixExpression, arguments) =>
        val cudaArgList = List(dg, db, ns.getOrElse(0), stream.getOrElse(0))
        val cudaCallStr = "<<<" + cudaArgList.mkString(", ") + ">>>"
        generateExpression(varEnv, funEnv)(postfixExpression) + cudaCallStr + arguments.map(generateExpression(varEnv, funEnv)).mkString("(", ", ", ")")
      case other => super.generateExpression(varEnv, funEnv)(other)
    }
  
}