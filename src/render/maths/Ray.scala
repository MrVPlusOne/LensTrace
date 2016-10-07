package render.maths

import scala.swing.Color

case class Ray(origin:Event,direction:Vector3,maxRecursive:Int=1){
  def getPoint(deltaT: Double): Vector3 = origin.space+direction*deltaT
}

