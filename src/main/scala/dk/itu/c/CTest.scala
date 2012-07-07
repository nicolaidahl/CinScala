package dk.itu.c


object Test extends Generator with App {
  
  def getEmptyVarEnv: Map[String, Type] = Map.empty[String, Type]
  def getEmptyFunEnv: Map[String, (Option[Type], ArgList)] = Map.empty[String, (Option[Type], ArgList)]
  
  def mainFunctionWrapper(stmtOrDecs: List[StmtOrDec]): FunctionDec =
    FunctionDec(Some(TypeInteger), "main", List((TypeInteger, "argc"), (TypePointer(TypePointer(TypeChar)), "args")), stmtOrDecs)
  
  //Tests the main function generation with if and return statements
  def mainFunctionTest: Program = {
	val locVarAss = new LocalVariableWithAssign(TypeInteger, "testVar", ConstantInteger(2))
	val ifstmt = If(AccessExpr(AccessVariable("testVar")), Return(Some(ConstantInteger(2))), Return(Some(ConstantInteger(5))))
    
    val statements = List(locVarAss, Stmt(ifstmt))
    val mainFunc = mainFunctionWrapper(statements)
    
    Program(List(mainFunc))
  }
  //println(generate(mainFunctionTest, getEmptyVarEnv, getEmptyFunEnv))
  
  //Testing that variable declaration and assignment works
  def variableDec: Program = {
    val locVar = LocalVariable(TypeInteger, "testVar")
    val assign = Assign(AccessVariable("testVar"), ConstantInteger(2))
    val locVar2 = LocalVariableWithAssign(TypeInteger, "testVar2", ConstantInteger(3))
    
    val statements = List(locVar, Stmt(ExpressionStatement(assign)), locVar2)
    val mainFunc = mainFunctionWrapper(statements)
    
    Program(List(mainFunc))
  }
  //println(generate(variableDec, getEmptyVarEnv, getEmptyFunEnv))
  
  
  //Testing variable scope for access to variable in unreachable scope
  def variableScope: Program = {
    val locVar = LocalVariable(TypeInteger, "testVar")
    val block = Block(List(locVar))
   
    val assign = Assign(AccessVariable("testVar"), ConstantInteger(2))
    
    val statements = List(Stmt(block), Stmt(Return(Some(assign))))
    val mainFunc = mainFunctionWrapper(statements)
    
    Program(List(mainFunc))

  }
  //println(generate(variableScope, getEmptyVarEnv, getEmptyFunEnv))
  
  //Testing redefinition of variable
  def variableRedefinition: Program = {
    val locVar = LocalVariable(TypeInteger, "testVar")
    val assign = Assign(AccessVariable("testVar"), ConstantInteger(2))
    val locVar2 = LocalVariableWithAssign(TypeInteger, "testVar", ConstantInteger(3))
    
    val statements = List(locVar, Stmt(ExpressionStatement(assign)), locVar2)
    val mainFunc = mainFunctionWrapper(statements)
    
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
    val mainFunc = mainFunctionWrapper(mainStatements)
    
    Program(List(testFun, mainFunc))

  }
  println(generate(multipleFunctions, getEmptyVarEnv, getEmptyFunEnv))
  
  
}




