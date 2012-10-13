package dk.itu.cinscala

trait CompileAndRunInterface {
  type Program
  type Prog
  type Result
  
  val commandRunner = CommandRunner
  
  def compile(abstractSyntaxTree: Program, varEnv: List[String], funEnv: List[String]): Prog
  def run(program: Prog): Result
  def compileAndRun(abstractSyntaxTree: Program, varEnv: List[String], funEnv: List[String]): Result
}

trait CCompileAndRun extends CompileAndRunInterface with CGenerator {
  import CAbstractSyntax._
  
  type Program = CProgram
  type Prog = String
  type Result = String
  
  def compile(abstractSyntaxTree: Program, varEnv: VarEnv = getEmptyVarEnv, funEnv: FunEnv = getEmptyFunEnv): Prog = {
	generate(abstractSyntaxTree, varEnv, funEnv)
  }
  
  def run(program: Prog): Result = {
    val fileName = "cProgram"
    commandRunner.writeToFile(fileName + ".c", program)
    
    //Compile and run
    val (_, stderr, exitCode) = commandRunner.run("gcc " + fileName + ".c -o " + fileName + ".o")
    stderr.size match {
      case 0 => {
        val (stdout, stderr1, exitCode1) = commandRunner.run("./" + fileName + ".o")
        stdout.mkString("\n")
      }
      case _ => throw new IllegalArgumentException(program +  "\n Does not compile: \n" + stderr.mkString("\n"))
    } 
  }
  
  def compileAndRun(abstractSyntaxTree: Program, varEnv: List[String] = getEmptyVarEnv, funEnv: List[String] = getEmptyFunEnv): Result =
    run(compile(abstractSyntaxTree, varEnv, funEnv)) 
}

object CUDACompileAndRun extends CompileAndRunInterface with CUDAGenerator {
  import CAbstractSyntax._
  import CUDAAbstractSyntax._
  
  type Program = CUDAProgram
  type Prog = String
  type Result = String
  
  def compile(abstractSyntaxTree: Program, varEnv: VarEnv = getEmptyVarEnv, funEnv: FunEnv = getEmptyFunEnv): Prog = {
	generate(abstractSyntaxTree, varEnv, funEnv)
  }
  
  def run(program: Prog): Result = {
    val fileName = "cudaProgram"
    commandRunner.writeToFile(fileName, program)
    
    //Compile and run
    val (_, stderr, exitCode) = commandRunner.run("nvcc -O3 " + fileName + ".cu -o " + fileName)
    stderr.size match {
      case 0 => {
        val (stdout, stderr1, exitCode1) = commandRunner.run("./" + fileName)
        stdout.mkString("\n")
      }
      case _ => stderr.mkString("\n")
    } 
  }
  
  def compileAndRun(abstractSyntaxTree: Program, varEnv: List[String] = getEmptyVarEnv, funEnv: List[String] = getEmptyFunEnv): Result =
    run(compile(abstractSyntaxTree, varEnv, funEnv)) 
}