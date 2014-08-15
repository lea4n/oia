import scala.io._
import scala.util.matching.Regex
import java.text.SimpleDateFormat

class Patterns {
  
  def isDate = false
  
  val patterns = Map(
    "Default" -> new Regex(".*")
  )

  def get(): Map[String, Regex] = {
    patterns
  }
}

class Date() extends Patterns() {
  override val patterns = Map(
    "yyyy-MM-dd HH:mm:ss" -> new Regex("[2-9][0-9]{3}-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]:[0-5][0-9]"),
    "MMM dd, yyyy H:mm:ss a" -> new Regex("[A-Z][a-z]{2} [0-3][0-9], [2-9][0-9]{3} [0-9]{1,2}:[0-5][0-9]:[0-5][0-9] [AP]M"),
    "M/dd/yy H:mm:ss:SSS" -> new Regex("[0-9]{1,2}/[0-3][0-9]/[0-9]{2} [0-9]{1,2}:[0-5][0-9]:[0-5][0-9]:[0-9]{3}")
  )
  
  override def isDate = true;
}

class Exception() extends Patterns() {
  override val patterns = Map(
    "Exception" -> new Regex("[a-zA-Z.]+Exception")
  )
}

class LogInfo() extends Patterns() {
  override val patterns = Map(
    "Info" -> new Regex(" (ERROR|WARN|E|W) ")
  )
}

class File() extends Patterns() {
  override val patterns = Map(
    "File" -> new Regex("[a-zA-Z]+\\.java")
  )
}

object Test {

  def main(args: Array[String]) {

    val separator = ","
    
    var lstData:List[String] =  List()
    
    val patterns:List[Patterns] = List(new Date(), new LogInfo(), new Exception())
    
    var hasDate = false
    
    for (line <- Source.stdin.getLines) {

      if(lstData.length == 0){
        hasDate = false
      } else if( hasDate && (lstData.length >= 2)) {
        println(lstData.toString())
        lstData = List()
      }
      
      for(pattern <- patterns) {
        pattern.get().keys.foreach {
          key =>
          var str = ( pattern.get()(key) findAllIn line).mkString(separator)
          
          if(str.length>0) {
            if(pattern.isDate) {
              if (hasDate == false) {
                val fmt = new SimpleDateFormat(key)
                str = fmt.parse(str).toString()
              
                hasDate = true
                lstData = lstData ::: List(str)
              }
            } else {
              lstData = lstData ::: List(str)
            }
          }
        }
      }
      
      if(hasDate && (lstData.length > 1)) {
        println(lstData.toString())
        lstData = List()
      }
    }
  }
}
