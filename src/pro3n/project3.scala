object project3 {
import scala.actors.Actor
import scala.actors.Actor._
import scala.util.Random
import scala.collection.mutable._
import scala.util.control.Breaks._
import java.security.MessageDigest

val BASE = 16 

case class join(joinID:String,level:Int,key:Int) 
case class newNode(newID:String, newkey:Int)
case class route(rtID:String, hop:Int)
case class rtForm(level:Int, rtlevel:Array[(String,Int)])
case class neForm(neS:MutableList[(String,Int)])
case class lfForm(lfS:Array[(String,Int)],nID: String,nKey: Int)
case class findhash(nodeID:Int)
case class hashvalue(rtID:String)
case class HOP(hop:Int)
case class close

class pastryNode(pubKey:Int,Pa:Array[pastryNode],Boss:Actor,nodeID:String,numrequest:Int) extends Actor {
    //val nodeID = "aa"
    //val pubKey= PubKey
    var status= "down";
    val table = new Array[Array[(String,Int)]](8, 16)
    var neSet = new MutableList[(String,Int)]();
    var lfSet = new Array[(String,Int)](16);
  def act{
    var Msnum=0
    var nearKey=pubKey-1;  
       if(nearKey>=0)Pa(nearKey) ! join(nodeID,0,pubKey)
       else Boss ! "ready"
     /*for (m <- 1 until pubKey+1) {
    	if(Pa(pubKey-m).status=="alive") {
    	Pa(pubKey-m) ! join(nodeID,0,pubKey);
    	break;
    	   }  
        }    */
    loop{
      react{
	       case "close" => exit()
         case join(joinID,level,key) => 
          if (lfSet.contains(joinID,key))  Pa(key) ! lfForm(lfSet,nodeID,pubKey);
          else{  
            val nextHop=lookUpRt(joinID)
            //println(nextHop.toString);
          if(nextHop!=pubKey) {
                Pa(nextHop) ! join(joinID,level+1,key)
             if(level>0)   {
             	val l=Prelen(joinID,nodeID);
             	Pa(key) ! rtForm(l,table(l)); 
             	Pa(key) ! newNode(nodeID,pubKey); 
             	 }
             else if (level==0) {Pa(key) ! neForm(neSet);Pa(key) ! newNode(nodeID,pubKey); }
           }
          else     Pa(key) ! lfForm(lfSet,nodeID,pubKey);
        }
        // update        
        case newNode(newID, newkey) =>  
             updateWithNew(newID, newkey)
        // route request
        case route(rtID, hop)=>
             var i=0;
		         var f = 1
		       // self  
          if(rtID==nodeID) {  Boss ! HOP(hop); 
          	               // println("here over")
                               self ! "route"; }
            else{
           // whether in lfSet 	
            while(i < lfSet.length && f==1) {
		        if(lfSet(i)!=null && lfSet(i)._1 ==rtID)  
                {
                  Boss ! HOP(hop+1)
                 // println("find in lfSet "+rtID+" with Hop="+hop.toString)
                  self ! "route";     
                  f=0;
                 }
                 i+=1;
               }
           // not in lfSet , whether in neSet or RT   
          if(f == 1){
             val nextHop=lookUpRt(rtID)
            // println(nextHop.toString);
          if(nextHop!=pubKey) {
          	Pa(nextHop) ! route(rtID,hop+1)
          	//println(pubKey.toString+"to"+nextHop.toString+"with"+rtID)
          	//println(pubKey.toString+"  to  "+nextHop.toString+" with " +rtID +" : "+(hop+1).toString)
          }
          else {
          	Boss ! HOP(hop)
          	self ! "route"; 
            }
          }
        }
         // newnode to form routetable and sets
        case rtForm(level, rtlevel)=> 
            // updateWithNew(sender.nodeID, sender.pubKey)
             table(level)= rtlevel
              
        case neForm(neS) => 
            // updateWithNew(sender.nodeID, sender.pubKey);
             neSet = neS
             
        case lfForm(lfS,nID, nKey ) =>         
             for(i<-0 until lfSet.length) lfSet(i)=lfS(i)  
             updateWithNew(nID,nKey);        
             broadCast;
             status="alive"
             Boss ! "ready"
	          // println(pubKey.toString+"is ready")
         // route    
        case "route" => 
        // route message to self with node ID
            if (Msnum < numrequest) {
            var t=Random.nextInt(Pa.length) 
            while(Pa(t).status!= "alive"|| t==pubKey){
          	t=Random.nextInt(Pa.length) }
            Boss ! findhash(t) // find the nodeID
            Msnum+=1;
          }
           else if (Msnum==numrequest) {Boss ! "finish" ; Msnum+=1}
          
            
         
        case hashvalue(rtID) => 
        // get the target hash value from boss and begin route
          
             var f=1
             for (i <- 0 until lfSet.length) {
		         if(lfSet(i)!=null && lfSet(i)._1 ==rtID)  
                { 
                  Boss ! HOP(1);
                  self ! "route"; 
                  f=0;
                 }
               }
             if(f == 1){
             val nextHop=lookUpRt(rtID)
          //   println(nextHop.toString);
          if(nextHop!=pubKey) {
          	Pa(nextHop) ! route(rtID,1)
          	//println("New Start+ "+pubKey.toString+"  to  "+nextHop.toString+" with  "+rtID)
          }
          else {
          	self ! "route"; 
          	Msnum-=1
            }
          }
          
              
        case close => exit()
       }
     }
   }

   def lookUpRt(rtID:String):Int={
   	// route table found item
     val prelen = Prelen(rtID,nodeID)
     if(prelen==8) return pubKey
     val  d = Dis(nodeID, rtID)
    // println("rtID: "+rtID +"| nodeID: "+ nodeID)
     val curr=table(prelen)(Integer.parseInt(rtID.charAt(prelen).toString,16))
     if (curr!=null) { 
     	  //println(curr._2.toString+"in table row "+prelen.toString);
     	  return curr._2;
     	  }
     else {
     	// cannot find in table(rare case), so in R+L+N to seek closest
     	var nexthop= 0;
     	// lfSet found closest
     	for (i <- 0 until lfSet.length){
     	      if (lfSet(i)!=null){ 
     	      val nodeij =lfSet(i)
     	      if (Dis(rtID, nodeij._1)<d && Prelen(rtID, nodeij._1)>= prelen) {
                nexthop = nodeij._2;
                return nexthop
             }
            }
           }
       // neSet found closest   
        for (i <- 0 until neSet.length){
     	      val nodeij =neSet(i)
     	      if (Dis(rtID, nodeij._1)<d && Prelen(rtID, nodeij._1)>= prelen) {
                nexthop = nodeij._2;
               return nexthop
             }
           }
        for (i<- 0 until table.length; j<-0 until table(0).length){  
            if (table(i)(j)!=null){
            	val nodeij =table(i)(j)
              if (Dis(rtID, nodeij._1)<d  && Prelen(rtID, nodeij._1)>=prelen) {
               nexthop = nodeij._2;
               return nexthop
           }
         }  
       }
       return pubKey
    }
  }
  
  def Prelen(A: String, B:String): Int= A.zip(B).takeWhile(Function.tupled(_ == _)).map(_._1).mkString.length
  def Dis(A: String, B:String): Long= (java.lang.Long.parseLong(A,16)- java.lang.Long.parseLong(B,16)).abs
  def comp(A:String,B:String):Boolean ={
  	if(((java.lang.Long.parseLong(A,16))- java.lang.Long.parseLong(B,16))>0) return true;
  	else return false;
    }
  def broadCast{
        for (i <- 0 until lfSet.length) 
             if (lfSet(i)!=null) Pa(lfSet(i)._2) ! newNode(nodeID, pubKey)
     	
        for (i <- 0 until neSet.length) Pa(neSet(i)._2) ! newNode(nodeID, pubKey)
     	 
        for (i<- 0 until table.length; j<-0 until table(0).length){  
            if (table(i)(j)!=null)   Pa(table(i)(j)._2) ! newNode(nodeID, pubKey)         
       }
   }

   def updateWithNew(rtID: String, key:Int){
     var f=1;
     var i=0;
 	   val newNode=(rtID, key)
     val prelen=Prelen(rtID,nodeID)
     if(prelen==8) return;
     val col = Integer.parseInt(rtID.charAt(prelen).toString,16)
     val curr=table(prelen)(col)
     if (curr==null) table(prelen)(col)=newNode
     if (lfSet.contains(rtID,key)||neSet.contains(rtID,key)) return;
     if (neSet.length<BASE) neSet+=newNode
     else{
     	while(i < BASE&& f==1) {
		        if((pubKey-neSet(i)._2).abs > (pubKey-key).abs)  
                { neSet(i) = newNode;  
                  f=0;
                 }
                 i+=1;
      }     
     }
     if(!comp(rtID,nodeID)){
     	if (lfSet((BASE/2)-1)!=null) {if(Dis(lfSet(findFar(0))._1,nodeID)>Dis(newNode._1,nodeID)) lfSet(findFar(0))=newNode}
     	else{
     		f=1;i=0;
       	while(i < BASE/2 && f==1) {
		        if(lfSet(i)==null)  
                { lfSet(i)=newNode
                  f=0;
                 }
                 i+=1;
               }
     	} 	
      }
    
      else{
      	if (lfSet(BASE-1)!=null) {if(Dis(lfSet(findFar(1))._1,nodeID)>Dis(newNode._1,nodeID)) lfSet(findFar(1))=newNode}
      	else{
      		 f=1; i=BASE/2
      		 while(i < BASE && f==1) {
		        if(lfSet(i)==null)  
                { lfSet(i)=newNode
                  f=0;
                 }
                 i+=1;
               }
     	}
    }
}
  def findFar(Set:Int):Int={
  	var maxL= 0.toLong
  	var j=0
  	if(Set==0){
  		for (i <- 0 until BASE/2){
  			val t=((java.lang.Long.parseLong(nodeID,16))- java.lang.Long.parseLong(lfSet(i)._1,16)).abs
  			if(t>maxL) maxL=t; j=i;
  		}
  		return j
  	}
  	else{
  		for (i <- BASE/2 until BASE){
  			val t=((java.lang.Long.parseLong(nodeID,16))- java.lang.Long.parseLong(lfSet(i)._1,16)).abs
  			if(t > maxL) maxL=t; j=i;
  		}
  		return j
  	}
    }
}
class Boss(numNodes:Int, numrequest:Int) extends Actor {
  def act{
     var sum = 0 ;
		 var n=0
  	 var readyNode = 0
  	 var endNode = 0
  	 val Pa= new Array[pastryNode](numNodes)
  	 val md = MessageDigest.getInstance("MD5")
         println("START create nodes ...")
  	 for(i <- 0 until numNodes){
     var text = i.toString
     var hash = md.digest(text.getBytes).map("%02x".format(_)).mkString.substring(0,8)
    // println(i.toString +" with "+ hash)
     Pa(i)= new pastryNode(i,Pa,self,hash,numrequest);
         }
         println("FINISH create nodes !")
	 println("START nodes join in the network ...")
     Pa(0).start
   loop{
      react{
        case "ready" =>       
        if (readyNode==numNodes-1){
           println("FINISH nodes join in the network !")
           println("START routing with "+numNodes.toString+" nodes and each send "+numrequest.toString)
         for( j<- 0 until numrequest){
             for(i  <- 0 until numNodes) Pa(i) ! "route" 
           }
        }
         else {      
          readyNode+=1;    
          Pa(readyNode).start 
          }
        case findhash(nodeID)=>
        var text = nodeID.toString
        var ha = md.digest(text.getBytes).map("%02x".format(_)).mkString.substring(0,8)
       // println(ha)
        sender ! hashvalue(ha)
        
        case "finish" =>
        endNode+=1;

        case HOP(hop)=>
	sum+=hop;
	n+=1;
        //println(n.toString)
             if (n==( numNodes* numrequest)){
		println("Result: sum "+sum.toString+" and n "+n.toString)
                println("So, average Hops: "+(sum.toDouble/n.toDouble).toString);	
			   for(i <- 0 until numNodes) {Pa(i) ! close;} 
                         exit()
			   }
		

       } 
     }
  }
 }

def main ( args:Array[String] ){
  val numNodes= Integer.parseInt(args(0))
  val numrequest = Integer.parseInt(args(1))
  val boss = new Boss(numNodes, numrequest);
  boss.start
  }
}
