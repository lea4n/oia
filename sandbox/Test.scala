import scala.io._
import scala.util.matching.Regex
import java.text.SimpleDateFormat

class Patterns {
  val patterns = Map(
    "Default" -> new Regex(".*")
  )

  def getPatterns(): Map[String, Regex] = {
    patterns
  }
}

class Date() extends Patterns() {
  override val patterns = Map(
    "yyyy-MM-dd HH:mm:ss" -> new Regex("[2-9][0-9]{3}-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-5][0-9]:[0-5][0-9]"),
    "MMM dd, yyyy H:mm:ss a" -> new Regex("[A-Z][a-z]{2} [0-3][0-9], [2-9][0-9]{3} [0-9]{1,2}:[0-5][0-9]:[0-5][0-9] [AP]M"),
    "M/dd/yy H:mm:ss:SSS" -> new Regex("[0-9]{1,2}/[0-3][0-9]/[0-9]{2} [0-9]{1,2}:[0-5][0-9]:[0-5][0-9]:[0-9]{3}")
  )
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

object Test {

  def main(args: Array[String]) {

    val separator = ","
    val date = new Date()
    val info = new LogInfo()
    val exception = new Exception()

    var lnDt = ""
    var lnExc = ""
    var lnInf = ""

    for (line <- Source.stdin.getLines) {

      info.getPatterns().keys.foreach {
        k =>
          val l = (info.getPatterns()(k) findAllIn line).mkString(separator)

          if (l.length > 0) {
            lnInf = l
          }
      }
      date.getPatterns().keys.foreach {
        k =>

          val l = (date.getPatterns()(k) findAllIn line).mkString(separator)

          if (l.length > 0) {
            val fmt = new SimpleDateFormat(k)
            lnDt = fmt.parse(l).toString()
          }
      }

      exception.getPatterns().keys.foreach {
        k =>

          val l = (exception.getPatterns()(k) findAllIn line).mkString(separator)

          if (l.length > 0) {
            lnExc = l
          }

          if ((lnDt.length > 0) && (lnExc.length > 0)) {
            println(lnDt + " " + lnInf + " " + lnExc)
            lnDt = ""
            lnExc = ""
          }
      }
    }
  }
}
