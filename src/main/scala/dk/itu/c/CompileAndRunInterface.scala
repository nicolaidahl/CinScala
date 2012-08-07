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
  
  type AST = Program
  type Prog = String
  type Result = String
  
  def compile(abstractSyntaxTree: AST): Prog = {
	generate(abstractSyntaxTree, getEmptyVarEnv, getEmptyFunEnv)
  }
  
  def run(program: Prog): Result = {
    val fileName = "cProgram"
    commandRunner.writeToFile(fileName, program)
    
    //Compile and run
    val (_, stderr, exitCode) = commandRunner.run("gcc " + fileName + ".c -o " + fileName + ".o")
    val (stdout, stderr1, exitCode1) = commandRunner.run("./" + fileName + ".o")
    stdout.mkString("\n")
  }

}



object CUDACompileAndRun extends CompilerAndRunInterface with CUDAGenerator {

  type AST = Program
  type Prog = String
  type Result = String
  
  def compile(abstractSyntaxTree: AST): Prog = {
	""
  }
  
  def run(program: Prog): Result = {
    ""
  }
  
  
}