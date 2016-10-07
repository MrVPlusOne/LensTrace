package render.gui

import render.objects.{RadianceSet, Radiance}

/**
 * Created by wjydzh1 on 12/23/2014.
 */
class ScreenModel(val width:Int,val height:Int) {
  def cut(col:Int,row:Int)={
    val dw=width/col
    val dh=height/row
//    val lastw=width-(dw*(col-1))
//    val lasth=height-(dh*(row-1))
    (for(i<-0 until row;j<-0 until col) yield{
      ScreenPortion(
        xFrom = j*dw,
        yFrom = i*dh,
        xTo = /*if(j==col-1) width else*/ (j+1)*dw,
        yTo = /*if(i==row-1) height else*/ (i+1)*dh
      )
    }).toArray
  }
}
case class ScreenPortion(xFrom:Int,xTo:Int,yFrom:Int,yTo:Int){
  val width=xTo-xFrom
  val height=yTo-yFrom
  val portionData:Array[Array[RadianceSet]]=Range(0,height).map(_=>Range(0,width).map(_=>RadianceSet.zero).toArray).toArray
}
