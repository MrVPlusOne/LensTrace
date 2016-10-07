package render.maths

import render.objects.Radiance

case class Vector3(x:Double,y:Double,z:Double){
  def lengthSquared=x*x+y*y+z*z
  def length=math.sqrt(lengthSquared)
  def normalized= {
    val len=length
    assert(len!=0,"zero Vector3 can't be normalized!")
    Vector3(x/len,y/len,z/len)
  }
  def -(v:Vector3)=Vector3(x-v.x,y-v.y,z-v.z)
  def unary_- =Vector3(-x,-y,-z)
  def +(v:Vector3)=Vector3(x+v.x,y+v.y,z+v.z)
  def *(f:Double)=Vector3(x*f,y*f,z*f)
  def /(f:Double)={
    val inv=1.0/f
    this * inv
  }
  def dot (v:Vector3)=x*v.x+y*v.y+z*v.z
  def cross(v:Vector3)=Vector3( v.z*y - v.y*z, -(v.z*x) + v.x*z, v.y*x - v.x*y )
  def multiply(v:Vector3)=Vector3(x*v.x,y*v.y,z*v.z)

  def closeTo(other:Vector3): Boolean ={
    (this-other).lengthSquared<Vector3.defaultTolerance
  }
  
  def apply(i:Int):Double=i match{
    case 0 => x
    case 1 => y
    case 2 => z
    case _ => throw new IndexOutOfBoundsException
  }
  
  def map(f:Double=>Double)=Vector3(f(x),f(y),f(z))
  def this(array: Array[Double])=this(array(0),array(1),array(2))
}

object Vector3{
  val defaultTolerance=1e-10

  val unit=Vector3(1,1,1)
  val front=Vector3(0,0,-1)
  val right=Vector3(1,0,0)
  val up=Vector3(0,1,0)
  val left=Vector3(-1,0,0)
  val down=Vector3(0,-1,0)
  val back=Vector3(0,0,1)
  val zero=Vector3(0,0,0)
  val positiveInf=Vector3(Double.PositiveInfinity,Double.PositiveInfinity,Double.PositiveInfinity)
}

case class Event(space:Vector3,t:Double)

object Event{
  val origin=Event(Vector3.zero,0)
  val positiveInf=Event(Vector3.positiveInf,Double.PositiveInfinity)
}

case class Vector2(x:Double,y:Double) {
  def rotate(rad: Double) = {
    val sin=math.sin(rad)
    val cos=math.cos(rad)
    this.complexTimes(Vector2(cos,sin))
  }

  def complexTimes(v: Vector2):Vector2={
    Vector2(x*v.x-y*v.y,x*v.y+y*v.x)
  }

  def *(d: Double) = Vector2(x*d,y*d)

  def /(d:Double)={
    val inv=1.0/d
    this * inv
  }

  def +(v:Vector2)=Vector2(x+v.x,y+v.y)

  def -(v:Vector2)=Vector2(x-v.x,y-v.y)

  def dot(v:Vector2)= x*v.x+y*v.y

  def lengthSquared = x*x+y*y

  def length=math.sqrt(lengthSquared)
}

object Vector2{
  val zero=Vector2(0,0)
  val right=Vector2(1,0)
  val up=Vector2(0,1)
  val oneone=Vector2(1,1)
}