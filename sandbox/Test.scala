import scala.io._
import scala.util.matching.Regex

class Patterns {
  val patterns = Map(
    "Default" -> new Regex(".*")
  )
  
  def getPatterns() : Map[String,Regex] = {
    return patterns
  }
}

class Date() extends Patterns() {
  override val patterns = Map(
    "YYYY-mm-dd HH:MM:SS" -> new Regex("[2-9][0-9]{3}-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]:[0-5][0-9]"),
    "MMM dd, YYYY H:MM:SS aaa" -> new Regex("[A-Z][a-z]{2} [0-3][0-9], [2-9][0-9]{3} [0-9]{1,2}:[0-5][0-9]:[0-5][0-9] [AP]M")
  ) 
}

class Exception()  extends Patterns() {
  override val patterns = Map(
    "Exception" -> new Regex("[a-zA-Z.]+Exception")
  )
}
object Test {
  
  def main(args: Array[String]) {

    var line = ""

    var date = new Date()
    var exception = new Exception()
        
    for (line <- Source.stdin.getLines) {
        
      date.getPatterns().keys.foreach { 
        k => 
        
        var l = (date.getPatterns()(k) findAllIn line).mkString("|")
        
        if(l.length>0) println(l + " Match With:" + date.getPatterns()(k) + " Date Formater:" + k )
      }   
      
      exception.getPatterns().keys.foreach { 
        k => 
        
        var l = (exception.getPatterns()(k) findAllIn line).mkString(",")
        
        if(l.length>0) println(l + " Match With:" + exception.getPatterns()(k) + " Exception Formater:" + k )
      }
    }
  }
}