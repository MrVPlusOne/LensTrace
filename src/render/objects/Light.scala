package render.objects

import com.sun.org.apache.xpath.internal.operations.Div
import render.maths.{Vector2, Const, Vector3, Ray}

abstract class Light {
  def radianceAt(ray: Ray): Radiance

  def radianceSetAt(ray:Ray) : RadianceSet=RadianceSet(radianceAt(ray),Vector2.zero)
}

class ConeLight(dir:Vector3,coneDegree:Double,radiance: Radiance) extends Light {
  val invDirec= -dir.normalized
  val minCos=math.cos(coneDegree*Const.DegToPi)
  override def radianceAt(ray: Ray): Radiance =
    if(ray.direction.dot(invDirec)>minCos) radiance
    else Radiance.zero
}

class ConeChangingLight(dir:Vector3,centric:Double,radiance:Radiance) extends Light{
  val invDirec= -dir.normalized
  override def radianceAt(ray: Ray): Radiance = {
    def square(d:Double)=d*d

    val deg=math.acos(ray.direction.dot(invDirec))
    val p=math.exp(-square(deg*centric)).toFloat
    radiance*p
  }
}

case class Radiance(r:Float,g:Float,b:Float){
  def apply(i:Int):Float=i match{
    case 0=>r
    case 1=>g
    case 2=>b
  }

  def dot(that: Radiance) = Radiance(r*that.r,g*that.g,b*that.b)

  def /(div: Float) =Radiance(r/div,g/div,b/div)

  def + (that:Radiance)=Radiance(r+that.r,g+that.g,b+that.b)

  def * (d:Float)= Radiance(r*d,g*d,b*d)

  def componentSum:Float=r+g+b
}

object Radiance{
  val red=Radiance(1,0,0)
  val blue=Radiance(0,0,1)
  val green=Radiance(0,1,0)
  val white=Radiance(1,1,1)
  val zero=Radiance(0,0,0)
}

case class RadianceSet(radiance: Radiance,complex:Vector2){
  def *(f:Float):RadianceSet=RadianceSet(radiance*f,complex*f)

  def +(that:RadianceSet)=RadianceSet(radiance+that.radiance,complex+that.complex)
}
object RadianceSet{
  val waveLength=10
  val lamda=1 //wave length in the spectrum
  val k= 2*math.Pi/waveLength
  val color=Radiance(1,0,0)

  val zero=RadianceSet(Radiance.zero,Vector2.zero)

  implicit def Radiance2Set(radiance: Radiance):RadianceSet=RadianceSet(radiance,Vector2.zero)
}