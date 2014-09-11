object project2 {

import scala.actors.Actor
import scala.actors.Actor._
import scala.util.Random
import scala.collection.mutable._

case class im_over(rID: Int)

class pushsumNode(neighbor:MutableList[Int], id:Int, Stap:Array[pushsumNode], boss: Actor ) extends Actor {
  def act{
    var neighbor2=neighbor
    var s:Double = id.toDouble
    var w:Double = 1.0
    var r0:Double = id.toDouble
    var converge_count:Int = 0
    loop{
      react{
        case "start" =>
         var t = neighbor2(Random.nextInt(neighbor2.length))
          s=s/2 
          w=w/2
          Stap(t) ! (s, w)    
         // println("from"+id+"to"+t+"with s="+s+";  w="+w)
        case "close"=> exit()
        case (sm:Double, wm:Double) =>
          s = (sm+s)/2
          w = (wm+w)/2
          var tt = neighbor2(Random.nextInt(neighbor2.length))         
         // println("from  "+id+" to "+tt+" with s= "+s+"  ;w= "+w)
          if (math.abs(s/w-r0)<1e-10) converge_count+=1
          else converge_count=0
         // println("r;diff="+(s/w)+" ; "+(s/w-r0)+" count=  "+converge_count)
          Stap(tt) ! (s, w)
          r0 = s/w
          if (converge_count==3){
           
            //for (i <- 0 until neighbor2.length) Stap(neighbor2(i)) ! im_over(id)
            boss ! (im_over(id),r0)
            println("Actor No."+id.toString+" is over by count")
            exit()
          }
          }       
       }
    }
}
class gossipNode(neighbor:MutableList[Int], id:Int, Sta:Array[gossipNode],boss: Actor) extends Actor {
  def act{
    var neighbor2 = neighbor
    var rum_num = 0
    loop {
      if(rum_num>0) {
      val t = neighbor2(Random.nextInt(neighbor2.length))
 	    Sta(t) ! "gossip" }
      react {
      case "gossip" =>
          if(rum_num == 0) {
          //println(id+"knows the truth")
          boss ! "Igot"; }
          rum_num += 1
          if(rum_num ==10) {
          //	println(id.toString+" is over by count")
            for (i <- 0 until neighbor2.length) Sta(neighbor2(i)) ! id; 
           // boss ! im_over(id)
            exit()
          }
        case "close"=> exit()
        
        case (rID:Int) =>
          neighbor2 = neighbor2 filter ( _ != rID)
          if(neighbor2.isEmpty) {
          //println(id.toString+" is over by isolation")
           //  boss ! im_over(id)
          exit()
          }
       }
     }
  }
}
class starter(numNodes:Int, topology:String, algorithm: String) extends Actor {
  def act{
     var accOverNode = 0
     val len = math.sqrt(numNodes).toInt
     val Sta = new Array[gossipNode](numNodes)
     val Stap = new Array[pushsumNode](numNodes)
    topology match {    
	  case "2D" =>	      
        for (i <- 0 until numNodes) {
        var neigh=new MutableList[Int]()
        if (i-len >= 0)  neigh+=(i-len)
        if (i+len < numNodes)  neigh+=(i+len)
				if (i%len != 0) neigh+=(i-1)
				if ((i+1)%len != 0 ) neigh+=(i+1)        
        algorithm match{
        case "gossip"=>
        Sta.update(i, new gossipNode(neigh, i, Sta,self))
        case "pushsum"=>
        Stap(i) = new pushsumNode(neigh, i, Stap,self)
         }
        }      
    case "line" =>
        for (i <- 0 until numNodes) {
        var neigh=new MutableList[Int]()
        if (i != 0) neigh += (i-1)
        if (i!=(numNodes-1))  neigh+=(i+1)
        algorithm match{
        case "gossip"=>
        Sta(i) = new gossipNode(neigh, i, Sta,self)
        case "pushsum"=>
        Stap(i) = new pushsumNode(neigh, i, Stap,self)
      }      
    }
     case "imp2D" =>	      
        for (i <- 0 until numNodes) {
        var neigh=new MutableList[Int]()
        if (i-len >= 0)  neigh+=(i-len)
        if (i+len < numNodes)  neigh+=(i+len)
				if (i%len != 0) neigh+=(i-1)
				if ((i+1)%len != 0 ) neigh+=(i+1)
				
        var Rest2DNode=new MutableList[Int]()
        for (j <- 0 until numNodes) Rest2DNode+=j
        Rest2DNode = Rest2DNode.diff(neigh)
        val ranNode = Rest2DNode(Random.nextInt(Rest2DNode.length))
        neigh+=ranNode                  //random node
        
        algorithm match{
        case "gossip"=>
        Sta.update(i, new gossipNode(neigh, i, Sta,self))
        case "pushsum"=>
        Stap(i) = new pushsumNode(neigh, i, Stap,self)
         }
        }
        
        case "full" =>
        for (i <- 0 until numNodes) {
        var neigh=new MutableList[Int]()
        for (j <- 0 until i) neigh+=j
        for (j <- i+1 until numNodes) neigh+=j
        algorithm match{
        case "gossip"=>
        Sta(i) = new gossipNode(neigh, i, Sta,self)
        case "pushsum"=>
        Stap(i) = new pushsumNode(neigh, i, Stap,self)
      }      
    }
}
   algorithm match{
        case "gossip"=>
           for (i <- 0 until numNodes)  Sta(i).start
        case "pushsum"=>
           for (i <- 0 until numNodes)  Stap(i).start
      }      
   val b = System.currentTimeMillis
        algorithm match{
        case "gossip"  =>   Sta((numNodes/2).toInt) ! "gossip"
        case "pushsum"=>   Stap((numNodes/2).toInt) ! "start"
        }  
 
    loop{
      react{
        case (im_over(rID),resultt) =>           
            println(b-System.currentTimeMillis)
            println(resultt)
            for (i <- 0 until numNodes)  Stap(i)! "close"
           exit()
        case "Igot" =>
             accOverNode+=1
             if (accOverNode == numNodes)  
             {println(b-System.currentTimeMillis+"ms used")
             	for (i <- 0 until numNodes)  Sta(i)! "close"
             	println("gossip has been successfully spread")
             	println("gossip has been successfully spread")
             	println("gossip has been successfully spread")
             
           exit()}        
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
