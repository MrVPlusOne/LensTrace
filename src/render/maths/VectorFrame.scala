package render.maths

/**
 * Created by wjydzh1 on 1/1/2015.
 */
class VectorFrame(ux:Vector3,uy:Vector3,uz:Vector3) {
  def localToGlobal(v:Vector3)= ux*v.x+uy*v.y+uz*v.z
  def globalToLocal(v:Vector3)= Vector3(v dot ux,v dot uy,v dot uz)
}

object VectorFrame{
  def fromNormal(n:Vector3)= {
    val up = n
    val rightUnormed: Vector3 = Vector3.up cross up
    val length=rightUnormed.length
    if(length==0.0){
      new VectorFrame(Vector3.right,Vector3.up,Vector3.back)
    }else {
      val right = rightUnormed/length
      val back = right cross up
      new VectorFrame(right, up, back)
    }
  }
}
