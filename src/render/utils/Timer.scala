package render.utils
import scala.actors.Actor._

class Timer(deltaInMillis:Long,action:Long=>Unit) {
  var isRunning=false
  def start(): Unit ={
    if(isRunning)
      return

    isRunning=true
    val startTime=System.currentTimeMillis()
    actor{
      while(isRunning){
        action(System.currentTimeMillis()-startTime)
        Thread.sleep(deltaInMillis)
      }
    }
  }

  def stop(): Unit ={
    isRunning=false
  }
}
