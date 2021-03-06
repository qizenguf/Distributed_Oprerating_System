package acl

import java.io._
import java.util._
import java.text._
import scala.actors.Actor
import scala.actors.Actor._
import scala.util.Random
import scala.collection.mutable._
import scala.util.control.Breaks._
import java.security.MessageDigest
import java.io._
import java.util._
import java.text._

abstract class ActorLogging extends Actor {
      private var count = 0
      val foldername = "logfile_bonus"
      val file:File = new File(foldername)
      val name = "Actor"
      file.mkdirs()
      val fw = new BufferedWriter(new FileWriter(foldername + "/" + name))

    override def react(handler: PartialFunction[Any, Unit]): Nothing = {
        val handler2: PartialFunction[Any, Unit] = {
            case x =>
                println("Inside Logs -- with message recieved  -- "+x);
                logFile( x.toString, "Actor_"+sender.toString)
                handler.apply(x);
        }
        super.react(handler2)
    }
    def logFile(m: String, from : String){
      count+=1
      fw.write("[INFO] " + "type: Got, "  + "name: " + name + ", " + "number: " + count)
      fw.newLine
      fw.write("[TIME] " + getTime)
      fw.newLine
      fw.write("[MESSAGE] " + m)
      fw.newLine
      fw.write("[FROM] " + from)
      fw.newLine
      fw.newLine
      fw.flush
      }
       def getTime : String = {
      val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
      dateFormat.format(new Date())
     }
        
   }
