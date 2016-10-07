package render.maths

import render.maths.Matrix4.Array2D

class Matrix4(val elem: Array2D){
  assert(elem.length==4)
  assert(elem.forall(_.length==4))

  def * (m:Matrix4):Matrix4 = {
    val array=new Array2D(4)
    for(i<-0 until 4){
      array(i)=new Array[Double](4)
      for(j<-0 until 4){
        array(i)(j) = (0 until 4).map(k=>elem(i)(k)*m.elem(k)(j)).sum
      }
    }
    new Matrix4(array)
  }

  def displayRow(row:Array[Double])=row.mkString(",")
  override def toString= "Matrix4[\n%s;\n%s;\n%s;\n%s;\n]".format(
    displayRow(elem(0)),
    displayRow(elem(1)),
    displayRow(elem(2)),
    displayRow(elem(3))
  )
}

object Matrix4{
  type Array2D = Array[Array[Double]]

  private val zeroArray = (0 until 4).map(i=> Array(0.0,0.0,0.0,0.0)).toArray
  private val identityArray = (0 until 4).map( i=>(0 until 4).map(j=>if(j!=i)0.0 else 1.0).toArray).toArray
  val zero=new Matrix4(zeroArray)
  val ident=new Matrix4(identityArray)
  
  def copyArray(original:Array[Array[Double]])={
    val array=new Array2D(4)
    for(i<-0 until 4){
      array(i)=new Array[Double](4)
      for(j<-0 until 4) array(i)(j)=original(i)(j)
    }
    array
  }
  
  def diagonal(d1:Double,d2:Double,d3:Double,d4:Double):Matrix4={
    val diags=Array(d1,d2,d3,d4)
    val array=new Array2D(4)
    for(i<-0 until 4){
      array(i)=new Array[Double](4)
      for(j<-0 until 4) array(i)(j)=if(i==j) diags(i) else 0.0
    }
    new Matrix4(array)
  }
}
