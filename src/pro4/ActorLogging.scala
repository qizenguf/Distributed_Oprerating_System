package acl
import logging.Logging
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
     val log= new Logging("Actor"+this.toString)
    override def react(handler: PartialFunction[Any, Unit]): Nothing = {
        val handler2: PartialFunction[Any, Unit] = {
            case x =>
                println("Inside Logs -- with message recieved  -- "+x);
                log.Got(x.toString+ "  from  " +sender.toString)
                handler.apply(x);
        }
        super.react(handler2)
    }
}
