package render.maths

/**
 * Created by wjydzh1 on 12/16/2014.
 */
trait RelaTransform {
  def transformPoint(p:Vector3):Vector3
  def transformLightDirec(v:Vector3):Vector3
  def transformEvent(e:Event):Event
}

class LorenzTransform(speed:Double,direc:Vector3) extends RelaTransform{
  override def transformPoint(p: Vector3): Vector3 = ???

  override def transformLightDirec(v: Vector3): Vector3 = ???

  override def transformEvent(e: Event): Event = ???
}

class StaticTransform(val pos: Vector3,val lookAt:Vector3,val time:Double) extends RelaTransform{
  private val front=(lookAt-pos).normalized
  private val right= (front cross Vector3.up).normalized
  private val up=right cross front


  override def transformPoint(p: Vector3): Vector3 = p+pos

  override def transformLightDirec(v: Vector3): Vector3 =front *(-v.z)+up*v.y+right*v.x

  override def transformEvent(e: Event): Event = Event(transformPoint(e.space),time+e.t)
}


