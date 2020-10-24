import scala.io.Source
import java.io.File
import java.nio.file.{Paths, Files}
import java.io.PrintWriter

import scala.util.control.Breaks._

import scala.collection.mutable._

object DeadEndDetector extends App{
    final case class CommandLineArgumentException(
        private val message: String = "", 
        private val cause: Throwable = None.orNull
        ) extends Exception(message, cause) 

    // Initialize default input output files
    var fileIn = "Input.txt"   
    var fileOut = "Output.txt"
    // Read in command line args
    if(args.length != 2) {
        throw CommandLineArgumentException("use format: run [input.txt] [output.txt]")
    } else {
        fileIn = args(0) 
        fileOut = args(1)
    }   
    val writer = new PrintWriter(new File(fileOut))

    // Read input text file as graph edges
    def readGraph(fileName: String): List[Tuple2[Int,Int]] = {
        if(!Files.exists(Paths.get(fileName))){
            throw CommandLineArgumentException(fileIn + " not found.")
        }
        // Read file, ignore first line 
        val lines: List[String] = Source.fromFile(fileName).getLines.toList.drop(1).map(es => es.trim).filter(x => !x.isEmpty)
        // Make a list of tuples (int,int) of edges
        val graph: List[Tuple2[Int,Int]] = lines.map(x => x.split(" ")).map(x => (x(0).toInt,x(1).toInt) )
        
        graph
    }

    // Count number of edges for each node (also called the degree of a vertex)
    def getDegrees(edges:List[Tuple2[Int,Int]]): scala.collection.mutable.Map[Int,Int] = { 
        val dgs = scala.collection.mutable.Map[Int,Int]() 
        // Key: vertex, Value: number of edges (degree at that vertex)
        for (e <- edges) {
            e match{
                case (e1,_) if dgs.contains(e1) => dgs += (e._1 -> (dgs(e._1) + 1))
                case (e1,_) if !dgs.contains(e1)=> dgs += (e._1 -> 1)
                case _ => 
            }
            e match{
                case (_,e2) if dgs.contains(e2) => dgs += (e._2 -> (dgs(e._2) + 1))
                case (_,e2) if !dgs.contains(e2)=> dgs += (e._2 -> 1)
                case _ => 
            }
        }

        dgs
    }

    /* Trim the graph
        While there are still leaves in graph...
        1. record the leaves being removed at this step into var steps
        2. remove the leaves from the graph by removing them in var remainingEdges
        3. get the new leaves in the trimmed graph for the next iteration
        4. repeat step 1 until no leaves left
    */
    def trim(edges: List[Tuple2[Int, Int]]): (List[(Int,Int)],List[List[(Int,Int)]], List[List[(Int)]]) = {
        // Holds the degree, or number of edges at each node, of each vertex
        val dgs = getDegrees(edges)
        // Leaves (leaf nodes) to remove on this step of trimming
        var leaves: Map[Int,Int] = dgs.filter(x => x._2 == 1) 
        
        // Data to collect while trimming
        var r: List[(Int,Int)] = List() // r: local variable, edges removed on specific step of trimming
        var t: List[(Int,Int)] = edges  // t: edges trimmed from the graph (ends up being assinged to remainingEdges)
        var sE: List[List[(Int,Int)]] = List()
        var sV: List[List[(Int)]] = List()

        while(leaves.size > 0){
            val leafVertices  = leaves.keys
            sV = sV ++ List(leafVertices.toList)

            for(i <- leafVertices){
                r = r ++ t.filter(x => x._1 == i || x._2 == i)
                t = t.filter(x => x._1 != i && x._2 != i) 
            }
            sE = sE ++ List(r)
            r = List()
            leaves = getDegrees(t).filter(x => x._2 == 1)       
        }

        (t, sE, sV)
    }

    // Get the edges of potential trees (a graph consisting of only treees) in the graph
    def getTrees(removed: List[List[(Int,Int)]], remaining: List[(Int,Int)]): List[(Int,Int)] = {
        /* VARIABLES
        edgesTree: Will be the edges within all trees in the graph
                            - Initialized with all edges that have been removed
        cyclePathVerts: Will be the vertices within or connected to a cycle in the graph 
                            - Initiated with vertices in remainingEdges, which are all vertices within a cycle*/
        var edgesTree: List[(Int,Int)] = removed.reverse.flatten
        var cyclePathVerts = scala.collection.mutable.Set(remaining.map(x => List(x._1,x._2)).flatten: _*)
        
        // Remove the edges from the tree if they contain a vertex on a path with another vertex in a cycle
        breakable{
            while(true){
                var edgesInCyclePath = 0
                for(edge <- edgesTree){
                    if (!edgesTree.isEmpty && (cyclePathVerts.contains(edge._1) || cyclePathVerts.contains(edge._2))){
                        cyclePathVerts += edge._1; cyclePathVerts += edge._2
                        edgesTree = edgesTree.filter(_ != edge)
                        edgesInCyclePath += 1
                    }
                }
                if(edgesInCyclePath == 0) break
                edgesInCyclePath = 0
            }
        }

        edgesTree
    }

    def getSignsTrees(removedE: List[List[(Int,Int)]], removedV: List[List[(Int)]], treeGraphs: List[(Int,Int)]): List[(String)] = {
        // Edges first removed, or the leaves of the tree, will have signs
        val treeOnlySignEdges: List[(Int,Int)] = removedE(0).filter(x => treeGraphs.contains(x))
        // Sign should be on vertex that is the leaf node
        var treeOnlySignEdgesOrder: List[(Int,Int)] = List()
        for (edge <- treeOnlySignEdges){
            if (removedV(0).contains(edge._1)){
                treeOnlySignEdgesOrder = treeOnlySignEdgesOrder ++ List(edge)
            } else {
                treeOnlySignEdgesOrder = treeOnlySignEdgesOrder ++ List((edge._2,edge._1))
            }
        }
        val treeOnlySignEdgesStr: List[(String)] = treeOnlySignEdgesOrder.map(x => x._1.toString + " " + x._2.toString)
        
        treeOnlySignEdgesStr
    }

    def getSignsCycles(remainingE: List[(Int,Int)], removedE: List[List[(Int,Int)]]): List[(String)] = {
        var cycleVerts = scala.collection.mutable.Set(remainingE.map(x => List(x._1,x._2)).flatten: _*)
        // Edges that have one vertex in the cycle and one vertex out of the cycle are ones we should put a sign on
        val cycleGraphSignEdges: List[(Int,Int)]  = removedE.flatten.filter(x => 
                    (cycleVerts.contains(x._1) && !cycleVerts.contains(x._2)) || 
                    (!cycleVerts.contains(x._1) && cycleVerts.contains(x._2))
                    )
        // Sign should be on the vertex that is in the cycle
        var cycleGraphSignEdgesOrder: List[(Int,Int)] = List()
        for(edge <- cycleGraphSignEdges){
            if(cycleVerts.contains(edge._1)){
                cycleGraphSignEdgesOrder = cycleGraphSignEdgesOrder ++ List(edge)
            } else {
                cycleGraphSignEdgesOrder = cycleGraphSignEdgesOrder ++ List((edge._2,edge._1))
            }
        }
        val cycleGraphSignEdgesStr: List[(String)] = cycleGraphSignEdgesOrder.map(x => x._1.toString + " " + x._2.toString)

        cycleGraphSignEdgesStr
    }

    /*
    Finds dead ends. It returns the edges that need a dead end sign. 
    */
    def detectDeadEnds(edges: List[Tuple2[Int, Int]]): Unit = {
        // VARIABLES
        // remainingEdges: Graph after being trimmed to vertices that are in a cycle or to an empty graph
        // removedEdges: Leaves edges removed and on which step (enocded by index) of trimming
        // removedVertices: Leaves vertices removed and on which step (enocded by index) of trimming

        //Trim the graph and collect data
        val trimData = trim(edges)
        val remainingEdges: List[(Int,Int)] = trimData._1
        val removedEdges: List[List[(Int,Int)]] = trimData._2
        val removedVertices: List[List[(Int)]] = trimData._3
        

        // If removedEdges is empty, then there are no dead end signs (it is simply a cycle or empty graph)
        // If removedEdges is not empty, then there are dead end signs and we need to find where to put them
        if (!removedEdges.isEmpty){

            // Get trees, if any, in graph                   
            val treeEdges = getTrees(removedEdges, remainingEdges)
            
            (edges,remainingEdges,treeEdges) match {                   
                // Disconnected graph with both tree graphs and graphs that contain a cycle
                case(_,remaining,tree) if (!tree.isEmpty && !remaining.isEmpty) => {
                    // Get signs of graphs with cycles
                    val cycleGraphSignEdgesStr: List[(String)] = getSignsCycles(remainingEdges, removedEdges)
                    
                    // Get signs of tree only graphs
                    val treeOnlySignEdgesStr: List[(String)] = getSignsTrees(removedEdges, removedVertices, tree)

                    // Write the answer to a file
                    writer.write((cycleGraphSignEdgesStr.length + treeOnlySignEdgesStr.length).toString + "\n")
                    for(edgeWithSign <- cycleGraphSignEdgesStr){
                        writer.write(edgeWithSign + "\n")
                    }
                    for(edgeWithSign2 <- treeOnlySignEdgesStr){
                        writer.write(edgeWithSign2 + "\n")
                    }
                }
                // Only graphs that are trees
                case(_,remaining,tree) if (remaining.isEmpty) => {
                    // Get signs of tree only graphs
                    val treeOnlySignEdgesStr: List[(String)] = getSignsTrees(removedEdges, removedVertices, tree)
                    
                    // Write the answer to a file
                    for(edgeWithSign <- treeOnlySignEdgesStr){
                        writer.write(edgeWithSign + "\n")
                    }
                }
                // Only graphs that contain a cycle
                case(_,remaining,tree) if (tree.isEmpty && !remaining.isEmpty) => {
                     // Get signs of graphs with cycles
                    val cycleGraphSignEdgesStr: List[(String)] = getSignsCycles(remainingEdges, removedEdges)
                    
                    // Write the answer to a file
                    writer.write(cycleGraphSignEdgesStr.length.toString + "\n")
                    for(edgeWithSign <- cycleGraphSignEdgesStr){
                        writer.write(edgeWithSign + "\n")
                    }
                }
            }
        } 
        // No edges were removed so the graph is all cycles or there were no input edges
        else {
            writer.write("0")
        }

        writer.close()    
    }

    val startTime: Long = System.currentTimeMillis()
    val graphRead = readGraph(fileIn)
    detectDeadEnds(graphRead)
    val endTime: Long = System.currentTimeMillis()
    val runTime: Double = (endTime.toDouble - startTime.toDouble) / 1000
    
    println()
    println("Goal: Under 5 seconds")
    println("Your Runtime: " + runTime.toString + " seconds")
    if(runTime < 5.0) println("You did it!!!!!!!")
    else println("You're not very good at this :(")
    println()
}