object project2 {

import scala.actors.Actor
import scala.actors.Actor._
import scala.util.Random
import scala.collection.mutable._

//case class im_over(rID: Int)
//case class rumor

class staff(neighbor:MutableList[Int], id:Int, Sta:Array[staff] ) extends Actor {
  def act{
    var neighbor2=neighbor
    var rum_num = 0
    loop {
      if(rum_num>0) {
      val t = neighbor2(Random.nextInt(neighbor2.length))
 	Sta(t) ! "rumor"
      }
      react {
        case "rumor" =>
          rum_num += 1
          if(rum_num ==10) {
          	println(id.toString+" is over")
            for (i <- 0 until neighbor2.length) Sta(neighbor2(i)) ! id; 
            exit()
          }
        case (rID:Int) =>
          neighbor2 = neighbor2 filter ( _ != rID)
          if(neighbor2.isEmpty) {
          println(id.toString+" is over"); exit()
          }
      }
    }
  }
}

def main ( args:Array[String] ){
  val numNodes=Integer.parseInt(args(0))
  val len = math.sqrt(numNodes).toInt
  val topology=args(1)
  //var i= 0
  val Sta= new Array[staff] (numNodes)
    topology match {
	  case "grid" =>	      
        for (i <- 0 until numNodes) {
        var neigh=new MutableList[Int]()
        if (i-len >= 0)  neigh+=(i-len)
        if (i+len < numNodes)  neigh+=(i+len)
				if (i%len != 0) neigh+=(i-1)
				if ((i+1)%len != 0 ) neigh+=(i+1)
        Sta(i) = new staff(neigh, i, Sta)
        }      
        for (i <- 0 until numNodes)  Sta(i).start
    case "line" =>
        for (i <- 0 until numNodes) {
        var neigh=new MutableList[Int]()
        if (i != 0) neigh += (i-1)
        if (i!=(numNodes-1))  neigh+=(i+1)
        Sta(i) = new staff(neigh, i, Sta)
        }      
        for (i <- 0 until numNodes)  Sta(i).start
}
   Sta(0) ! "rumor"
}
}
