object Project1 {

import scala.actors.Actor
import scala.actors.Actor._
case object Stop
//import java.math
class Calc extends Actor {
       def act {
          loop {
           react {
          case (r: Int, leng: Int) => sumUp(r,leng)
          case Stop => {
          judge ! Stop 
          exit()}
        }
      }
    }
     def sumUp(m:Int,n: Int){
          val t1= (m+n-1).toLong
          val t2= (m-1).toLong
          val sum = t1*(t1+1)*(2*t1+1)/6-t2*(t2+1)*(2*t2+1)/6
          judge ! (sum, m)       
              }
}

object judge extends Actor {
    def act{
          loop {
           react {
           case (sum: Long, m: Int ) =>
           {
           if(IsSquare(sum)) println("result = "+m.toString)
           //else println(m.toString)
           }
           case Stop => exit()
           }
        }
     }
      def IsSquare(sum: Long ): Boolean= {
      val root=math.sqrt(sum)
      //return root*root==sum
      return root.intValue==root   
    }
    }
    def main ( args:Array[String] ){
        val range=Integer.parseInt(args(0))
        val leng=Integer.parseInt(args(1))
        //println(range.toString)
       // println(leng.toString)
        //prIntln(range.toString)
        //prIntln(leng.toString)
       judge.start
       val CalOne = new Calc;
       val CalTwo = new Calc;
       val CalThree = new Calc;
       CalOne.start
       CalTwo.start
       CalThree.start
       for(start <- 0 until range/3){
       CalOne !(3*start+1,leng)
       CalTwo !(3*start+2,leng)
       CalThree !(3*start+3,leng)
       }
       for (s <- 3*(range/3) until range)  CalOne !(s,leng)
       CalTwo! Stop
       CalThree ! Stop
       CalOne! Stop
       
      
      }
 
 }