package render.geomeries

import render.maths.{Vector2, Vector3, Ray,InvGeoTransform}
import render.objects.{HitInstance, HitCheckResult}

case class GeoInfo(normal:Vector3, isOut:Boolean,uvPoint:Vector2){
  def localToGlobal(invGeoTrans:InvGeoTransform)= GeoInfo(invGeoTrans.transformDirec(normal).normalized,isOut,uvPoint)
}

sealed abstract class HitCheck
object Missed extends HitCheck
case class Hit(distance:Double,isOutside:Boolean,customData:Any=null) extends HitCheck

abstract class Geometry {
  def geoInfo(hitPoint: Vector3, isOutside: Boolean, customData:Any):GeoInfo
  def checkHit(origin:Vector3,dir:Vector3):HitCheck
}

object UnitSphere extends Geometry{
  override def geoInfo(hitPoint: Vector3, isOutside: Boolean, customData:Any): GeoInfo = {
    val normal=if(isOutside) hitPoint else -hitPoint

    val longitude=math.atan2(hitPoint.x,hitPoint.z)/(2*math.Pi)+0.5
    val latitude=math.asin(hitPoint.y)/math.Pi+0.5
    new GeoInfo(normal,isOutside,Vector2(longitude,latitude))
  }

  override def checkHit(origin:Vector3,dir:Vector3): HitCheck = {
    val project= -(origin dot dir)
    val disSquare=origin.lengthSquared-project*project
    if(disSquare<1) {
      val delta=math.sqrt(1-disSquare)
      if(project-delta>0) Hit(project-delta,true)
      else if(project+delta>0) Hit(project+delta,false)
      else Missed
    }
    else Missed
  }
}

object Cylinder extends Geometry{
  object Side
  object Bottom
  object Top

  override def geoInfo(hitPoint: Vector3, isOutside: Boolean, customData:Any): GeoInfo = {
    customData match{
      case Side=>CylinderSide.geoInfo(hitPoint,isOutside,null)
      case Top=>UnitDisk.geoInfo(hitPoint+Vector3.down,isOutside,null)
      case Bottom=>UnitDisk.geoInfo(hitPoint,!isOutside,null)
    }
  }

  override def checkHit(origin: Vector3, dir: Vector3): HitCheck = {
    val SideHit:HitCheck={
      def hitPointHeightOk(time:Double)={
        val hitPoint=origin+dir*time
        hitPoint.y>0&&hitPoint.y<1
      }
      CylinderSide.checkHit(origin,dir) match{
        case Hit(dis,isOut,data) if hitPointHeightOk(dis)=>Hit(dis,isOut,Side)
        case _=>Missed
      }
    }
    val BottomHit:HitCheck=UnitDisk.checkHit(origin,dir) match{
      case Hit(dis,outSide,customData)=>Hit(dis,!outSide,Bottom)
      case Missed=>Missed
    }
    val TopHit:HitCheck=UnitDisk.checkHit(origin+Vector3.down,dir) match{
      case Hit(dis,outSide,customData)=>Hit(dis,outSide,Top)
      case Missed=>Missed
    }
    List(SideHit,BottomHit,TopHit).minBy{
      case hit:Hit=>hit.distance
      case Missed=>Double.PositiveInfinity
    }
  }
}

private object UnitDisk extends Geometry{
  override def geoInfo(hitPoint: Vector3, isOutside: Boolean, customData:Any): GeoInfo = Plane.geoInfo(hitPoint,isOutside,null)

  override def checkHit(origin: Vector3, dir: Vector3): HitCheck = {
    Plane.checkHit(origin,dir) match{
      case h:Hit=>{
        val Vector3(x,_,z)=origin+dir*h.distance
        if(x*x+z*z<1) h
        else Missed
      }
      case Missed=>Missed
    }
  }
}

object CylinderSide extends Geometry{
  import render.maths.{Vector2 => Vec2}
  override def geoInfo(hitPoint: Vector3, isOutside: Boolean, customData:Any): GeoInfo = {
    val normal=if(isOutside) Vector3(hitPoint.x,0,hitPoint.z) else Vector3(-hitPoint.x,0,-hitPoint.z)
    val longitude=math.atan2(hitPoint.x,hitPoint.z)/(2*math.Pi)+0.5
    val height=hitPoint.z
    new GeoInfo(normal,isOutside,Vector2(longitude,height))
  }

  override def checkHit(origin: Vector3, dir: Vector3): HitCheck = {
    val r2=Vec2(origin.x,origin.z)
    val v2=Vec2(dir.x,dir.z)
    val projectSpeed=v2.length
    val vdir=v2/projectSpeed
    val project= -(r2 dot vdir)
    val disSquare= r2.lengthSquared - project*project
    if(disSquare<1){
      val delta=math.sqrt(1-disSquare)
      if(project-delta>0) Hit((project-delta)/projectSpeed,true)
      else if(project+delta>0) Hit((project+delta)/projectSpeed,false)
      else Missed
    }
    else Missed
  }
}

object Plane extends Geometry{
  override def geoInfo(hitPoint: Vector3, isOutside: Boolean, customData:Any): GeoInfo = {
    val normal=if(isOutside) Vector3.up else Vector3.down
    new GeoInfo(normal,isOutside,Vector2(hitPoint.x,hitPoint.z))
  }

  override def checkHit(origin: Vector3, dir: Vector3): HitCheck = {
    val dis=origin dot Vector3.up
    val speed=dir dot Vector3.up
    if(speed!=0){
      val t= -dis/speed
      if(t>0) Hit(t,if(origin.y>0) true else false)
      else Missed
    }else Missed
  }
}

object UnitSquare extends Geometry{
  override def geoInfo(hitPoint: Vector3, isOutside: Boolean, customData:Any): GeoInfo = Plane.geoInfo(hitPoint,isOutside,null)

  override def checkHit(origin: Vector3, dir: Vector3): HitCheck = {
    Plane.checkHit(origin,dir) match{
      case h:Hit=>{
        val Vector3(x,_,z)=origin+dir*h.distance
        if(x>0&&x<1&&z>0&&z<1) h
        else Missed
      }
      case Missed=>Missed
    }
  }
}

class Triangle(p0:Vector3,p1:Vector3,p2:Vector3) extends Geometry{
  val edge1= p1-p0
  val edge2= p2-p0
  val normal= edge1.cross(edge2).normalized
  override def geoInfo(hitPoint: Vector3, isOutside: Boolean, customData:Any): GeoInfo = {
    val n=if(isOutside) normal else -normal
    val delta=hitPoint-p0
    val uvPoint=Vector2(delta dot edge1,delta dot edge2)
    new GeoInfo(n,isOutside,uvPoint)
  }

  override def checkHit(origin: Vector3, dir: Vector3): HitCheck = {
    val s=origin-p0
    val s1=dir cross edge2
    val s2= s cross edge1
    val div=1.0/s1.dot(edge1)
    val t=s2.dot(edge2)*div
    if(t>0){
      val b1= div*s1.dot(s)
      val b2= div*s2.dot(dir)
      if(0<b1&&0<b2&&b1+b2<1) Hit(t,s.dot(normal)>0)
      else Missed
    }else Missed
  }
}

object TriangularMeshes{
  val pyramid:List[Triangle]={
    val bot1=Vector3.left
    val bot2=Vector3.right
    val bot3=Vector3.front*math.sqrt(3)
    val top=Vector3.front*(1/math.sqrt(3))+Vector3.up*math.sqrt(8.0/3)
    List(new Triangle(bot1,bot2,top),new Triangle(bot2,bot3,top),
      new Triangle(bot3,bot1,top),new Triangle(bot1,bot2,bot3))
  }
}
