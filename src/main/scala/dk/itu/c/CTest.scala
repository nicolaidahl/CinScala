package dk.itu.c


object Test extends CCompileAndRun with App {
  
  
  def generateMainFunction(stmtOrDecs: List[StmtOrDec]): FunctionDec =
    FunctionDec(Some(TypeInteger), "main", List((TypeInteger, "argc"), (TypePointer(TypePointer(TypeChar)), "args")), stmtOrDecs)
  
  //Tests the main function generation with if and return statements
  def mainFunctionTest: Program = {
	val locVarAss = new LocalVariableWithAssign(TypeInteger, "testVar", ConstantInteger(2))
	val ifstmt = If(AccessExpr(AccessVariable("testVar")), List(Stmt(Return(Some(ConstantInteger(2))))), None, Some(List(Stmt(Return(Some(ConstantInteger(5)))))))
    
    val statements = List(locVarAss, Stmt(ifstmt))
    val mainFunc = generateMainFunction(statements)
    
    Program(List(mainFunc))
  }
  //println(generate(mainFunctionTest, getEmptyVarEnv, getEmptyFunEnv))
  
  //Testing that variable declaration and assignment works
  def variableDec: Program = {
    val locVar = LocalVariable(TypeInteger, "testVar")
    val assign = Assign(AccessVariable("testVar"), ConstantInteger(2))
    val locVar2 = LocalVariableWithAssign(TypeInteger, "testVar2", ConstantInteger(3))
    
    val statements = List(locVar, Stmt(ExpressionStatement(assign)), locVar2)
    val mainFunc = generateMainFunction(statements)
    
    Program(List(mainFunc))
  }
  //println(generate(variableDec, getEmptyVarEnv, getEmptyFunEnv))
  
  
  //Testing variable scope for access to variable in unreachable scope
  def variableScope: Program = {
    val locVar = LocalVariable(TypeInteger, "testVar")
    val block = Block(List(locVar))
   
    val assign = Assign(AccessVariable("testVar"), ConstantInteger(2))
    
    val statements = List(Stmt(block), Stmt(Return(Some(assign))))
    val mainFunc = generateMainFunction(statements)
    
    Program(List(mainFunc))

  }
  //println(generate(variableScope, getEmptyVarEnv, getEmptyFunEnv))
  
  //Testing redefinition of variable
  def variableRedefinition: Program = {
    val locVar = LocalVariable(TypeInteger, "testVar")
    val assign = Assign(AccessVariable("testVar"), ConstantInteger(2))
    val locVar2 = LocalVariableWithAssign(TypeInteger, "testVar", ConstantInteger(3))
    
    val statements = List(locVar, Stmt(ExpressionStatement(assign)), locVar2)
    val mainFunc = generateMainFunction(statements)
    
    Program(List(mainFunc))

  }
  //println(generate(variableRedefinition, getEmptyVarEnv, getEmptyFunEnv))
  
  //Testing call of function in scope
  def multipleFunctions: Program = {
    val locVar = LocalVariable(TypeInteger, "testVar")
    val assign = Assign(AccessVariable("testVar"), ConstantInteger(2))
    val locVar2 = LocalVariableWithAssign(TypeInteger, "testVar1", ConstantInteger(3))
    
    val statements = List(locVar, Stmt(ExpressionStatement(assign)), locVar2)
    val testFun = FunctionDec(Some(TypeInteger), "testFunction", List((TypePointer(TypeInteger), "input")), statements)
    
    val funcCall = Call("testFunction", List(ConstantInteger(2)))
    val mainStatements = List(Stmt(ExpressionStatement(funcCall)))
    val mainFunc = generateMainFunction(mainStatements)
    
    Program(List(testFun, mainFunc))

  }
  //println(generate(multipleFunctions, getEmptyVarEnv, getEmptyFunEnv))
  
  //Testing redefinition of function
  def redefinitionOfFunction: Program = {
    
    val testFun = FunctionDec(Some(TypeInteger), "testFunction", List((TypePointer(TypeInteger), "input")), List())
    val testFun2 = FunctionDec(Some(TypeInteger), "testFunction", List((TypePointer(TypeInteger), "input")), List())
    
    val funcCall = Call("testFunction", List(ConstantInteger(2)))
    val mainStatements = List(Stmt(ExpressionStatement(funcCall)))
    val mainFunc = generateMainFunction(mainStatements)
    
    Program(List(testFun, testFun2, mainFunc))

  }
  //println(generate(redefinitionOfFunction, getEmptyVarEnv, getEmptyFunEnv))
  
  //Testing switch case statement
  def switchTest: Program = {
    
	val ifstmt = If(ConstantInteger(1), List(Stmt(Return(Some(ConstantInteger(2))))), None, Some(List(Stmt(Return(Some(ConstantInteger(5)))))))
    val plusExpr = BinaryPrim(BinaryPlus, ConstantInteger(2), ConstantInteger(4))
	
	val switchCase = Switch(ConstantInteger(1), List((ConstantInteger(1), List(Stmt(ExpressionStatement(plusExpr))))), Some(List(Stmt(ifstmt))))
	
    val mainStatements = List(Stmt(switchCase))
    val mainFunc = generateMainFunction(mainStatements)
    
    Program(List(mainFunc))

  }
  //println(generate(switchTest, getEmptyVarEnv, getEmptyFunEnv))
  
  println(compileAndRun(switchTest))
  
  
  
}




