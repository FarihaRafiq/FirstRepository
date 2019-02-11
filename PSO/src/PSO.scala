import scala.math
import scala.util.Random

object PSO {
  //Sphere Function Implementation
  def spherefunction(x : Int) : Int =  {
    var sum = 0
    for(i <- 1 to x)
      sum = sum + x(i) * 2
      return sum
  
  } 
  //Ackeley Function
  def ackeleyfunction(x: Int) : Double = {
    var a= 20
    var b = 0.2
    var c = 2.0 * 3.1428
    var sum1 = 0.0
    var sum2 = 0.0
    for (i <- 1 to x){
      sum1 = sum1 + x(i) * 2.0
      sum2 = sum2 + math.cos(c * x(i))
    }
    var d = x
      return -a * math.exp(-b * math.sqrt(sum1 / d)) - math.exp(sum2 / d) + a + math.E
    
  }
  //Sum Of Squares Functions
  def sumOfSquaresfunction(x : Int) : Int = {
    var sum = 0
    for (i <- 1 to x){
      sum = sum + i * x(i) * 2
    }
    return sum
  }
  
  class Particle() extends PSO {
     val position = List(Nil)          
     val velocity = List(Nil)        
     val best = List(Nil)          
     val err_best = -1          
     val err_i = -1               

        for (i <- 0 to Dimensions){
          
            val r = new scala.util.Random
            val start: Double = -1
            val end: Double   = 1
            val r1 = between(start, end, r)
            val rr = math.abs(r1)
            velocity.add (rr)
            position.add(rr)
        }
     //calculate random Position
     def between(low: Double, high: Double, r: Random): Double = {
       if (low == high) {
         low
         } 
       else {
         val mid = low + (high/2 - low/2)
         if (r.nextBoolean) between(low, mid, r)
         else between(mid, high, r)
       }
   }
     //evaluate fitness functions
     def evaluate(y : Int , UpperBound : Int)
        err_i= Up[position]

        //check to see if the current position is an individual best
        if (err_i < err_best){
            best = position
            err_best = err_i
        }
     // Update particle's velocity
     def update_velocity(y: Int , bestPos : Int){
        val weight : Double = 0.5      
        val c1 = 2       
        val c2 = 2        
        for (i <- 0 to pop_size){
            val r1 = new scala.util.Random
            val r2 = new scala.util.Random
            val Nostalgia= (c1) * (r1)* (bestPos - position(i))
            val Standard= c2 * r2* (pos_best_g - position(i))
            val velocity(i) = weight * velocity(i)+ Nostalgia+ Standard
        }
     }
     //Update Particle Position
     def updatePosition(y : Int, bounds : Int){
        for (i <- 0 to pop_size){
            position(i) = position(i) + velocity(i)

            if position(i) > bounds(i)[1]
                position(i) = bounds(i)[1]

            if position(i) < bounds(i)[0]
                position(i) = bounds(i)[0]
        }
     }
  }
  //PSO Class
  class PSO(y : Int, x : Int,Low: Int, Up : Int, pop_size : Int, maxiter : Int){
   
   var err_best_g = -1                   
   var pos_best_g= List(Nil) 
   var Dimensions : Int = x
   var global_best_cost = -1
   var Low : Int = -10
   var High : Int = 10
   var pop_size : Int = 10
   var maxiter : Int = 20
   
   var swarm = List(Nil)
        for (i <- 0 to pop_size){
            swarm = i :: swarm
        }
   var i=0
        while (i < maxiter){
            for (j <- 0 to pop_size){
              swarm(j).evaluate(Low, Up)

                // determine if current particle is the global best
                if (swarm(j).err_i < err_best_g or err_best_g == -1){
                    pos_best_g= List(swarm(j).position)
                    err_best_g= (swarm(j).err_i)
            }
            
        }
        
   for (j <- 0 to pop_size)
                swarm(j).update_velocity(pos_best_g)
                swarm(j).update_position(Low)
            i+=1

        
        println( "Computations are:", pos_best_g, err_best_g )

  }
  }
  def main(args: Array[String]) {
        
        val(x,y) = (12,13) 
        val UpperBound= 10
        val LowerBound = -10
       val population_size = 17
        val maxiteration = 30
        val obj1 = new PSO(spherefunction(6),x, UpperBound, LowerBound, population_size,maxiteration)
        val obj2 = new PSO(ackeleyfunction(15),x, UpperBound, LowerBound, population_size,maxiteration)
        val obj3 = new PSO(sumOfSquaresfunction(6),x, UpperBound, LowerBound, population_size,maxiteration)

}
}