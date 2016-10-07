package render.maths

trait InvGeoTransform {
  def transformPoint(vector3: Vector3):Vector3

  def transformDirec(vector3: Vector3):Vector3
}

object InvGeoTransform{
  type Array2D=Array[Array[Double]]

  def translation(offset:Vector3):InvGeoTransform={
    new GeoMixTransform(Vector3.unit,Rotation.Identity,offset)
  }
  def mix(scale:Vector3,orientation:Rotation,offset:Vector3)=new GeoMixTransform(scale,orientation,offset)
}

class GeoMixTransform(scale:Vector3,orientation:Rotation,offset:Vector3) extends InvGeoTransform{
  val needScale= scale!=Vector3.unit
  val needOffset= offset !=Vector3.zero
  
  override def transformDirec(v: Vector3): Vector3 = {
    val rotated=orientation(v)
    if(needScale) rotated multiply scale.map(1.0/_) else rotated
  }
  override def transformPoint(p: Vector3): Vector3 = {
    transformDirec(if(needOffset) p-offset else p)
  }
}

import InvGeoTransform.Array2D
/**
 * Deprecated
 */
class GeoMatrixTransform(mat:Array2D) extends InvGeoTransform{
  override def transformPoint(vector3: Vector3): Vector3 = {
    val v=new Array[Double](3)
    for(r<-0 until 3){
      val mr=mat(r)
      val dot=(0 until 3).map(c=> mr(c)*vector3(c)).sum+mr(3)
      v(r)=dot
    }
    new Vector3(v)
  }

  override def transformDirec(vector3: Vector3): Vector3 = {
    val v=new Array[Double](3)
    for(r<-0 until 3){
      val mr: Array[Double] = mat(r)
      val dot=(0 until 3).map(c=> mr(c) * vector3(c)).sum
      v(r)=dot
    }
    new Vector3(v)
  }
}
