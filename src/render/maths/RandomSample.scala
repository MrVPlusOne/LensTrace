package render.maths

import scala.collection.mutable
import scala.util.Random

abstract class RandomSample {
  def nextSampleOnHemisphere():Vector3 = {
    val v2=nextSample()
    val y=v2.x
    val r=math.sqrt(1-y*y)
    val angle=v2.y*2*math.Pi
    val cos: Double = math.cos(angle)
    val z=cos*r
    val sin=(if(angle>math.Pi) -1.0 else 1.0)*math.sqrt(1-cos*cos)
    val x=sin*r
    Vector3(x,y,z)
  }

  def getSample(turn: Int, group: Int ):Vector2=getData(turn,wrapInGroup(group))

  def nextSample():Vector2
  protected def getData(turn:Int,group:Int):Vector2
  protected def wrapInGroup(g:Int):Int

  def nextSampleInDisk():Vector2={
    val v=nextSample()*2-Vector2.oneone
    if(v.lengthSquared<1) v
    else nextSampleInDisk()
  }
}

/*
class MultiJittered(maxTurn:Int,groupNum:Int) extends RandomSample{

  def makeSample(): Array[Array[Vector2]] = {
    val random=new Random()
    def randomChoice(num:Int):Int= random.nextInt % num


  }

  val data:Array[Array[Vector2]]=makeSample()

  override def getSample(x: Int, y: Int, turn: Int): Vector2 = data(turn)((x+y)%groupNum)

}
*/
class SimpleRandom extends RandomSample{
  val random=new Random
  override def getData(turn: Int, group: Int ): Vector2 = nextSample()
  override def wrapInGroup(g:Int)=g

  override def nextSample(): Vector2 = Vector2(random.nextDouble(),random.nextDouble())
}