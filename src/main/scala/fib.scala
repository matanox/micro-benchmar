import scala.annotation.tailrec

abstract class Fib {
  def apply(): Double
  def runAndassert = assert(apply == 2.686381002448534E208)
}

object Streams extends Fib {
  def apply(): Double = {
    lazy val fibs: Stream[Double] = 0 #:: 1 #:: fibs
      .zip(fibs.tail)
      .map { case (fst, snd) => fst + snd }
  
    val realized = fibs.take(1000).force
    realized.last
  }
}

object ImmutableNotTailRec extends Fib {
  def apply(): Double = {
    
    /* store in reverse order to be fast on the list */
    def generate(l: List[Double]): List[Double] = {
      val next = l.head == 2.686381002448534E208 match {
        case true => l
        case false => generate(l.+:(l.head + l.tail.head)) 
      }
      next
    }
    
    val fibs = generate(List(1,0))
    
    fibs.head
  }
}

object ImmutableTailRec extends Fib {
  def apply(): Double = {
    
    /* store in reverse order to be fast on the list */
    @tailrec def generate(l: List[Double]): List[Double] = {
      if (l.head == 2.686381002448534E208) l
      else generate(l.+:(l.head + l.tail.head)) 
    }
    
    val fibs = generate(List(1,0))
    
    fibs.head
  }
}

object Plain2 extends Fib {
  def apply(): Double = {
    import collection.mutable.ArrayBuffer
    lazy val fibs: ArrayBuffer[Double] = ArrayBuffer(0,1)
    
    for (i <- 1 to 998)
      fibs += (fibs.last + fibs.apply(fibs.size-2))
  
    fibs.last
  }
}

object Plain1 extends Fib {
  def apply(): Double = {
    import collection.mutable.ArrayBuffer
    lazy val fibs: ArrayBuffer[Double] = ArrayBuffer(0,1)
    
    (1 to 998).foreach { _ =>
      fibs += (fibs.last + fibs.apply(fibs.size-2))
    }
    fibs.last
  }
}

object TimedExecution {
  def apply(f:Fib, times: Int) {
    require(times >= 1)
    val totalExecutionTime = 
      (1 to times).map { _ =>
        val startTime = System.nanoTime()
        f.runAndassert
        val endTime = System.nanoTime()
        val elapsed = (endTime - startTime)
        elapsed
      }.sum
      
    println(s"${totalExecutionTime.toFloat / times / 1000 / 1000} ms average elapsed time of ${f.getClass}")
  }
}

object Main extends App {
  val repeat = 100000
  Seq(ImmutableNotTailRec, ImmutableTailRec, Plain1, Plain2, Streams) foreach { 
    TimedExecution(_, repeat) 
  }
}
