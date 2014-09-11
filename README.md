Distributed_Oprerating_System
=============================

Distributed and multi-threaded projects with Scala.

Communication between different threads are implemented by messages exchange with actors.

Project 1:

  An interesting problem in arithmetic with deep implications to elliptic curve
  theory is the problem of finding perfect squares that are sums of consecutive
  squares. A classic example is the Pythagorean identity:
  3^2 + 4^2 = 5^2

  Use Scala and the actor model to build a good solution to this problem that runs
  well on multi-core machines.

Project 2:

  Gossip type algorithms can be used both for group communication and for aggregate
  computation. The goal of this project is to determinethe convergence of such 
  algorithms through a simulator based on actors writtenin Scala. Since actors in 
  Scala are fully asynchronous, the particular type of Gossip implemented is the so 
  called Asynchronous Gossip.
  
Project 3:

  The goal of this project is to implement in Scala using the actor model the Pastry
  protocol and a simple object access service to prove its usefulness.
  
  The specification of the Pastry protocol can be found in the paper 
  Pastry: Scalable, decentralized object location and routing for large-scale peer-to-
  peer systems. by A. Rowstron and P. Druschel. You can find the paper at
  http://research.microsoft.com/en-us/um/people/antr/PAST/pastry.pdf
  
  The paper above, in section 2.3 contains a specification of the Pastry API
  and of the API to be implemented by the application.
  
Project 4:

   You have complete freedom on what you can do for the project as long as you:

    a. Accomplish a non-trivial task. 
    b. Show usefulness via an example. 
    c. Produce a self-contained tool that does not require the user to use anything else
    to get the debugging done. Any visualization, extra analysis, etc. has to be automatically 
    started by your program.
    
    In this project, a logging function is implemented to log the message between different actors 
    and use MSCGen to plot the call graph. So the user can ZOOM in the graph and take a close look on the log 
    to detect the errors after a runtime failure.
    
    

  




