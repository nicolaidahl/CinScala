package dk.itu.c

trait CUDAGenerator extends CGenerator with CUDAAbstractSyntax {

  
  
  override def generate (prog: CProgram, varEnv: VarEnv, funEnv: FunEnv): String = {
    generateExternalDeclarations(varEnv, funEnv)(prog.contents)._3
  }
  
  /*override def generateTopDecs (varEnv: VarEnv, funEnv: FunEnv)(topDecs: List[ExternalDeclaration]): (VarEnv, FunEnv, String) = {
    topDecs match {
      case Nil => (varEnv, funEnv, "")
      case head :: tail =>
        head match {
          case CUDAFunctionDec(functionType, returnType, ident, params, stmtOrDecs) => 
            val functionTypeStr = generateCUDAFunctionType(functionType)
            val (funEnv1, str) = generateFunctionDec(varEnv, funEnv, CFunctionDec(returnType, ident, params, stmtOrDecs))
            val (varEnv1, funEnv2, str1) = generateTopDecs(varEnv, funEnv1)(tail)
            
            (varEnv1, funEnv2, functionTypeStr + " " + str + str1)
          case _ =>
            super.generateTopDecs(varEnv, funEnv)(tail)
        }
      
    }
  }*/
  
  /*def generateCUDAFunctionType(funcType: CUDAFunctionType): String =
    funcType match {
      case GlobalType() => "__global__"
      case DeviceType() => "__device__"
    }
  */
  
}