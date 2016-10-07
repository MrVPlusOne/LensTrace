package render.objects

import render.maths._

case class Camera(transform:StaticTransform,window:Vector2,maxRec:Int) {
  private val piFac=math.Pi/180
  private val tanX=math.tan(window.x*piFac)
  private val tanY=math.tan(piFac*window.y)

  def shootRay(coord:Vector2,localTime:Double):Ray={
    val localDirec=Vector3((2*coord.x-1)*tanX,(2*coord.y-1)*tanY,-1)
    val globalDirec=transform.transformLightDirec(localDirec).normalized
    val localEvent=Event(Vector3.zero,localTime)
    val globalEvent=transform.transformEvent(localEvent)
    Ray(globalEvent,globalDirec,maxRec)
  }
}

