package logging

import java.io._
import java.util._
import java.text._

class Logging(classname: String) {

  val foldername = "logfile"
  val file:File = new File(foldername)
  file.mkdirs()
  var count = 0
  
  val fw = new BufferedWriter(new FileWriter(foldername + "/" + classname))
  def Comment(t: String) = {
    count += 1
    fw.write("[INFO] " + "type: Comment, " + "name: " + classname + ", " + "number: " + count)
    fw.newLine
    fw.write("[TIME] " + getTime)
    fw.newLine
    fw.write("[TEXT] " + t)
    fw.newLine
    fw.newLine
    fw.flush
  }
  def Event = {
    count += 1
    fw.write("[INFO] " + "type: Event, "  + "name: " + classname + ", " + "number: " + count)
    fw.newLine
    fw.write("[TIME] " + getTime)
    fw.newLine
    fw.newLine
    fw.flush
  }
  def Fulfilled(c: String) = {
    count += 1
    fw.write("[INFO] " + "type: Fulfilled, "  + "name: " + classname + ", " + "number: " + count)
    fw.newLine
    fw.write("[TIME] " + getTime)
    fw.newLine
    fw.write("[CONDITION] " + c)
    fw.newLine
    fw.newLine
    fw.flush
  }
  def Got(m: String) = {
    count += 1
    fw.write("[INFO] " + "type: Got, "  + "name: " + classname + ", " + "number: " + count)
    fw.newLine
    fw.write("[TIME] " + getTime)
    fw.newLine
    fw.write("[MESSAGE] " + m)
    fw.newLine
    fw.newLine
    fw.flush
  }
  def Pipelined(m: String, c: String) = {
    count += 1
    fw.write("[INFO] " + "type: Pipelined, "  + "name: " + classname + ", " + "number: " + count)
    fw.newLine
    fw.write("[TIME] " + getTime)
    fw.newLine
    fw.write("[MESSAGE] " + m)
    fw.newLine
    fw.write("[CONDITION] " + c)
    fw.newLine
    fw.newLine
    fw.flush
  }
  def Problem(t: String, r: Exception) = {
    count += 1
    fw.write("[INFO] " + "type: Problem, "  + "name: " + classname + ", " + "number: " + count)
    fw.newLine
    fw.write("[TIME] " + getTime)
    fw.newLine
    fw.write("[TEXT] " + t)
    fw.newLine
    fw.write("[REASON] " + r)
    fw.newLine
    fw.newLine
    fw.flush
  }
  def Progressed(c: String) = {
    count += 1
    fw.write("[INFO] " + "type: Progressed, "  + "name: " + classname + ", " + "number: " + count)
    fw.newLine
    fw.write("[TIME] " + getTime)
    fw.newLine
    fw.write("[CONDITION] " + c)
    fw.newLine
    fw.newLine
    fw.flush
  }
  def Rejected(c: String, r: Exception) = {
    count += 1
    fw.write("[INFO] " + "type: Rejected, "  + "name: " + classname + ", " + "number: " + count)
    fw.newLine
    fw.write("[TIME] " + getTime)
    fw.newLine
    fw.write("[CONDITION] " + c)
    fw.newLine
    fw.write("[REASON] " + r)
    fw.newLine
    fw.newLine
    fw.flush
  }
  def Resolved(c: String) = {
    count += 1
    fw.write("[INFO] " + "type: Resolved, "  + "name: " + classname + ", " + "number: " + count)
    fw.newLine
    fw.write("[TIME] " + getTime)
    fw.newLine
    fw.write("[CONDITION] " + c)
    fw.newLine
    fw.newLine
    fw.flush
  }
  def Returned(m: String) = {
    count += 1
    fw.write("[INFO] " + "type: Returned, "  + "name: " + classname + ", " + "number: " + count)
    fw.newLine
    fw.write("[TIME] " + getTime)
    fw.newLine
    fw.write("[MESSAGE] " + m)
    fw.newLine
    fw.newLine
    fw.flush
  }
  def Sent(m: String) = {
    count += 1
    fw.write("[INFO] " + "type: Sent, "  + "name: " + classname + ", " + "number: " + count)
    fw.newLine
    fw.write("[TIME] " + getTime)
    fw.newLine
    fw.write("[MESSAGE] " + m)
    fw.newLine
    fw.newLine
    fw.flush
  }
  def SentIf(m: String, c: String) = {
    count += 1
    fw.write("[INFO] " + "type: SentIf, "  + "name: " + classname + ", " + "number: " + count)
    fw.newLine
    fw.write("[TIME] " + getTime)
    fw.newLine
    fw.write("[MESSAGE] " + m)
    fw.newLine
    fw.write("[CONDITION] " + c)
    fw.newLine
    fw.newLine
    fw.flush
  }

  def getTime : String = {
  val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
  dateFormat.format(new Date())
  }

}


