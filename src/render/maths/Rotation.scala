package render.maths

abstract class Rotation(degreeInRad:Double) {
  def apply(v:Vector3):Vector3
  val cos=math.cos(degreeInRad)
  val sin=math.sin(degreeInRad)
}

object Rotation{
  val Identity=new Rotation(0){
    override def apply(v:Vector3)=v
  }

  def rotate(axis:Vector3,rad:Double)=new Rotation(rad) {
    val ydir=axis.normalized

    override def apply(v: Vector3): Vector3 = {
      val ycomp= ydir dot v
      val vz=v - ydir * ycomp
      val zcomp=vz.length
      if(zcomp==0){
        v
      }else{
        val zdir=vz/zcomp
        val xdir=ydir cross zdir
        val newZ=zcomp*cos
        val newX=zcomp*sin
        ydir*ycomp+xdir*newX+zdir*newZ
      }
    }
  }
  def rotateX(rad:Double)=new Rotation(rad) {
    override def apply(v:Vector3)={
      val x=v.x
      val y=cos*v.y-sin*v.z
      val z=sin*v.y+cos*v.z
      Vector3(x,y,z)
    }
  }
  def rotateY(rad:Double)=new Rotation(rad) {
    override def apply(v: Vector3): Vector3 = {
      val y= v.y
      val z= cos*v.z-sin*v.x
      val x= sin*v.z+cos*v.x
      Vector3(x,y,z)
    }
  }
  def rotateZ(rad:Double)=new Rotation(rad) {
    override def apply(v: Vector3): Vector3 = {
      val x=cos*v.x-sin*v.y
      val y=sin*v.x+cos*v.y
      val z=v.z
      Vector3(x,y,z)
    }
  }
}