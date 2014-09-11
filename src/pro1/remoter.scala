object remoter {
import scala.actors.Actor
import scala.actors.Actor._
import scala.actors.remote._
import scala.actors.remote.RemoteActor._

case class answer(feedback:Int)
case class task(rg:Int, length:Int, group:Int, step:Int)
class myActor extends Actor {
     def act() {
	    alive(9000)
		register('myActor, self)
		loop{
		  react{
		  case task(rg, length, group, step) =>   
          val feedback = Compute(rg, length, group, step)
          sender ! answer(feedback)
          exit()}
		  }
	 }
  def Compute(range:Int,leng:Int, sp:Int, step:Int):Int ={
  var count=0;
  for(start <- sp until range by step) {
    val t1=(start+leng).longValue
    val t2=(start).longValue
    val root=math.sqrt((t1*(t1+1)*(2*t1+1)-t2*(t2+1)*(2*t2+1))/6)
    if(root.intValue==root)  { 
	  //println("Result: "+(start+1).toString) 
	  sender ! start+1
	  count += 1
	}
  }
  return count
}
}
def main ( args:Array[String] ){
  val myActor1 = new myActor
  myActor1.start	
}
}