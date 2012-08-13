package dk.itu.c


trait CompileAndRunInterface {
  this: CAbstractSyntax =>
  
  type Prog
  type Result
  
  val commandRunner = CommandRunner
  
  def compile(abstractSyntaxTree: Program): Prog
  def run(program: Prog): Result
  def compileAndRun(abstractSyntaxTree: Program): Result =
    run(compile(abstractSyntaxTree))
  
}


trait CCompileAndRun extends CompileAndRunInterface with CGenerator {
  
  type Prog = String
  type Result = String
  
  def compile(abstractSyntaxTree: Program): Prog = {
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



object CUDACompileAndRun extends CompileAndRunInterface with CUDAGenerator {

  type Prog = String
  type Result = String
  
  def compile(abstractSyntaxTree: Program): Prog = {
	generate(abstractSyntaxTree, getEmptyVarEnv, getEmptyFunEnv)
  }
  
  def run(program: Prog): Result = {
    val fileName = "cudaProgram"
    commandRunner.writeToFile(fileName, program)
    
    //Compile and run
    val (_, stderr, exitCode) = commandRunner.run("nvcc -O3 " + fileName + ".cu -o " + fileName)
    val (stdout, stderr1, exitCode1) = commandRunner.run("./" + fileName)
    stdout.mkString("\n")
  }
  
  
}