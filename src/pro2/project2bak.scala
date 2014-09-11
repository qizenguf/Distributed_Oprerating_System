object project2 {

import scala.actors.Actor
import scala.actors.Actor._
import scala.util.Random
import scala.collection.mutable._

case class im_over(rID: Int)
//case class rumor
case class push_sum(SS: Double, WW: Double)

class pushsumNode(neighbor:MutableList[Int], id:Int, Sta:Array[pushsumNode] ) extends Actor {
  def act{
    var neighbor2=neighbor
    var s:Double = id.toDouble
    var w:Double = 1.0
    var r0:Double = s/w
    var converge_count = 0
    loop{
      react{
        case "start" =>
         val t = neighbor(Random.nextInt(neighbor.length))
          Sta(t) ! push_sum(s, w)
        case push_sum(SS, WW) =>
          s = (SS+s)/2
          w = (WW+w)/2
          val t = neighbor(Random.nextInt(neighbor.length))
          Sta(t) ! push_sum(s, w)
          if ((s/w-r0)<1e-10) converge_count+=1
          else converge_count=0
          r0 = s/w
          if (converge_count==3){
            println("Actor No."+id.toString+" is over")
            for (i <- 0 until neighbor.length) Sta(neighbor(i)) ! im_over(id)
            boss ! im_over(id)
            exit()
          }
        case im_over(rID) =>
          neighbor2 = neighbor2 filter (_ != rID)
          if(neighbor.isEmpty){ 
            println("Actor No."+id.toString+" is over")
            boss ! im_over(id)
            exit()
          }       
       }
    }
  }
}
class rumorNode(neighbor:MutableList[Int], id:Int, Sta:Array[rumorNode] ) extends Actor {
  def act{
    var neighbor2=neighbor
    var rum_num = 0
    loop {
      if(rum_num>0) {
      val t = neighbor2(Random.nextInt(neighbor2.length))
 	Sta(t) ! "rumor" }
      react {
        case "rumor" =>
          rum_num += 1
          if(rum_num ==10) {
          	println(id.toString+" is over")
            for (i <- 0 until neighbor2.length) Sta(neighbor2(i)) ! id; 
            boss ! im_over(id)
            exit()
          }
        case (rID:Int) =>
          neighbor2 = neighbor2 filter ( _ != rID)
          if(neighbor2.isEmpty) {
          println(id.toString+" is over")
          boss ! im_over(id)
          exit()
          }
       }
     }
  }
}

class starter(numNodes:Int, topology:String, algorithm: String) extends Actor {
  def act{
      var acc = 0
      val len = math.sqrt(numNodes).toInt
     algorithm match{
        case "rumor"=>
        val Sta = new Array[rumorNode](numNodes)
        case "pushsum"=>
        val Sta = new Array[pushsumNode](numNodes)
       }
    topology match {                   //build topology
	  case "grid" =>	      
        for (i <- 0 until numNodes) {
        var neigh=new MutableList[Int]()
        if (i-len >= 0)  neigh+=(i-len)
        if (i+len < numNodes)  neigh+=(i+len)
				if (i%len != 0) neigh+=(i-1)
				if ((i+1)%len != 0 ) neigh+=(i+1)
        
        algorithm match{
        case "rumor"=>
        Sta(i) = new rumorNode(neigh, i, Sta)
        case "pushsum"=>
        Sta(i) = new pushsumNode(neigh, i, Sta)
         }
        }      
    case "line" =>
        for (i <- 0 until numNodes) {
        var neigh=new MutableList[Int]()
        if (i != 0) neigh += (i-1)
        if (i!=(numNodes-1))  neigh+=(i+1)
        algorithm match{
        case "rumor"=>
        Sta(i) = new rumorNode(neigh, i, Sta)
        case "pushsum"=>
        Sta(i) = new pushsumNode(neigh, i, Sta)
      }      
    }
  }

   for (i <- 0 until numNodes)  Sta(i).start
   val b = System.currentTimeMillis
        algorithm match{
        case "rumor"  =>   Sta(0) ! "rumor"
        case "pushsum"=>   Sta(0) ! "start"
        }  
 
    loop{
      react{
        case im_over(rID) =>
            acc +=1
            if (acc == numNodes){ 
            println(b-System.currentTimeMillis)
            exit()
             }
           }
        }   
     } 
  }

def main ( args:Array[String] ){
  val numN=Integer.parseInt(args(0))
  val topo= args(1)
  val algo = args(2)
  val boss = new starter(numN, topo, algo);
  boss.start
  }
}
