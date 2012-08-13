package dk.itu.c


trait CompilerAndRunInterface {
  
  type AST
  type Prog
  type Result
  
  val commandRunner = CommandRunner
  
  def compile(abstractSyntaxTree: AST): Prog
  def run(program: Prog): Result
  def compileAndRun(abstractSyntaxTree: AST): Result =
    run(compile(abstractSyntaxTree))
  
}


trait CCompileAndRun extends CompilerAndRunInterface with CGenerator {
  
  type AST = CProgram
  type Prog = String
  type Result = String
  
  def compile(abstractSyntaxTree: CProgram): Prog = {
	generate(abstractSyntaxTree, getEmptyVarEnv, getEmptyFunEnv)
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
      case _ => stderr.mkString("\n")
    } 
  }
}



object CUDACompileAndRun extends CompilerAndRunInterface with CUDAGenerator {

  type AST = CProgram
  type Prog = String
  type Result = String
  
  def compile(abstractSyntaxTree: CProgram): Prog = {
	""
  }
  
  def run(program: Prog): Result = {
    ""
  }
  
  
}