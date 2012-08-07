package dk.itu.c

class ScalaCLibrary {

}

object CommandRunner{
  import scala.sys.process.Process
  import scala.sys.process.ProcessLogger
  
  /** Run a command, collecting the stdout, stderr and exit status */
  def run(in: String): (List[String], List[String], Int) = {
    val qb = Process(in)
    var out = List[String]()
    var err = List[String]()

    val exit = qb ! ProcessLogger((s) => out ::= s, (s) => err ::= s)

    (out.reverse, err.reverse, exit)
  }
  
  def writeToFile(fileName: String, input: String) = {
    import java.io._
	
    def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
      val p = new java.io.PrintWriter(f)
      try { op(p) } 
      finally { p.close() }
    }
    
    val data = Array(input)
	printToFile(new File(fileName))(p => {
	  data.foreach(p.println)
	})
    
  }
}