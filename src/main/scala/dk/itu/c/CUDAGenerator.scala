package dk.itu.c

trait CUDAGenerator extends CGenerator with CUDAAbstractSyntax {

  
  
  override def generate (prog: CProgram, varEnv: VarEnv, funEnv: FunEnv): String = {
    generateExternalDeclarations(varEnv, funEnv)(prog.contents)._3
  }
  
  override def generateExternalDeclarations (varEnv: VarEnv, funEnv: FunEnv)(topDecs: List[CExternalDeclaration]): (VarEnv, FunEnv, String) = {
    topDecs match {
      case Nil => (varEnv, funEnv, "")
      case head :: tail =>
        head match {
          case CUDAFunctionDec(functionType, typeSpecifier, declarator, declarationList, compoundStmt) => 
            val functionTypeStr = generateCUDAFunctionQualifier(functionType)
            val (funEnv1, str) = generateFunctionDec(varEnv, funEnv, CFunctionDec(Some(CDeclarationSpecifiers(None, typeSpecifier, None)), declarator, declarationList, compoundStmt))
            val (varEnv1, funEnv2, str1) = generateExternalDeclarations(varEnv, funEnv1)(tail)
            
            (varEnv1, funEnv2, functionTypeStr + " " + str + str1)
          case _ =>
            super.generateExternalDeclarations(varEnv, funEnv)(tail)
        }
      
    }
  }
  
  def generateCUDAFunctionQualifier(funcType: CUDAFunctionQualifier): String =
    funcType match {
      case CUDAGlobalType() => "__global__"
      case CUDADeviceType() => "__device__"
      case CUDAHostType() => "__host__"
      case CUDANoInlineType() => "__noinline__"
    }
  
  override def generatePostfixExpression(varEnv: VarEnv, funEnv: FunEnv)(e: CPostfixExpression): String =
    e match {
      case CUDAKernelCall(dg, db, postfixExpression, arguments) =>
        ""
    }
  
}