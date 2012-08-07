package dk.itu.c

trait CUDAGenerator extends CGenerator with CUDAAbstractSyntax {

  
  
  override def generate (prog: Program, varEnv: VarEnv, funEnv: FunEnv): String = {
    generateTopDecs(varEnv, funEnv)(prog.contents)._3
  }
  
  override def generateTopDecs (varEnv: VarEnv, funEnv: FunEnv)(topDecs: List[TopDec]): (VarEnv, FunEnv, String) = {
    topDecs match {
      case Nil => (varEnv, funEnv, "")
      case head :: tail =>
        head match {
          case function: CUDAFunctionDec => 
            val functionTypeStr = generateCUDAFunctionType(function.functionType)
            val (funEnv1, str) = generateFunctionDec(varEnv, funEnv, function.funDec)
            val (varEnv1, funEnv2, str1) = generateTopDecs(varEnv, funEnv1)(tail)
            
            (varEnv1, funEnv2, functionTypeStr + " " + str + str1)
          case _ =>
            super.generateTopDecs(varEnv, funEnv)(tail)
        }
      
    }
  }
  
  def generateCUDAFunctionType(funcType: CUDAFunctionType): String =
    funcType match {
      case GlobalType() => "__global__"
      case DeviceType() => "__device__"
    }
  
  
}