package render.objects

import render.geomeries.{Hit, Missed, Geometry}
import render.maths._
import render.geomeries.GeoInfo

abstract class HitInstance {
  def hitCheck(ray:Ray):Option[HitCheckResult]
  def getRadianceSet(globalRay: Ray, scene: RelaScene, checkResult: HitCheckResult, sample: RandomSample, simplify: Int ):RadianceSet
  def globalToLocal(global:Event):Event
}

object HitInstance{
  val HitTolerance=1e-6
}
case class HitCheckResult(localEvent:Event,globalEvent:Event,isOut:Boolean,customData:Any)

class SimpleInstance(geo:Geometry,offset:Vector3,material:Mat) extends HitInstance{
  override def hitCheck(ray: Ray): Option[HitCheckResult] ={
    geo.checkHit(ray.origin.space-offset,ray.direction) match{
      case Missed=>None
      case Hit(dis,isOut,custom) if dis>HitInstance.HitTolerance=>{
        val globalPoint=ray.getPoint(dis)
        val localPoint=globalPoint-offset
        val t=dis+ray.origin.t
        Some(HitCheckResult(Event(localPoint,t),Event(globalPoint,t),isOut,custom))
      }
      case _=>None
    }
  }

  def getLocalRay(ray: Ray) = ray

  override def getRadianceSet(globalRay: Ray, scene: RelaScene, checkResult: HitCheckResult, sample: RandomSample, simplify: Int ): RadianceSet={
    val localPoint=checkResult.localEvent.space
    val isOut=checkResult.isOut
    val geoInfo=geo.geoInfo(localPoint,isOut,checkResult.customData)

    val localRay=getLocalRay(globalRay)
    material.shade(localRay, checkResult, geoInfo, scene, sample, simplify, this)
  }

  override def globalToLocal(global: Event): Event = Event(global.space-offset,global.t)
}

class ComplexInstance(geo:Geometry,invGeoTransform: InvGeoTransform,material:Mat) extends HitInstance{
  override def hitCheck(ray: Ray): Option[HitCheckResult] ={
    val rayOrigin=invGeoTransform.transformPoint(ray.origin.space)
    val rawDirec=invGeoTransform.transformDirec(ray.direction)
    val rayInvSpeed=1.0/rawDirec.length
    val rayDirec=rawDirec*rayInvSpeed
    
    geo.checkHit(rayOrigin,rayDirec) match{
      case Missed=>None
      case Hit(d,isOut,data) if d>HitInstance.HitTolerance =>{
        val dis=d*rayInvSpeed
        val globalPoint=ray.getPoint(dis)
        val localPoint=invGeoTransform.transformPoint(globalPoint)
        val t=dis+ray.origin.t
        Some(HitCheckResult(Event(localPoint,t),Event(globalPoint,t),isOut,data))
      }
      case _=>None
    }
  }

  def getLocalRay(ray: Ray) = ray

  override def getRadianceSet(globalRay: Ray, scene: RelaScene, checkResult: HitCheckResult, sample: RandomSample, simplify: Int ): RadianceSet={
    val localPoint=checkResult.localEvent.space
    val isOut=checkResult.isOut
    val localInfo=geo.geoInfo(localPoint,isOut,checkResult.customData)
    val geoInfo=localInfo.localToGlobal(invGeoTransform)

    val localRay=getLocalRay(globalRay)
    material.shade(localRay, checkResult, geoInfo, scene, sample, simplify, this)
  }

  override def globalToLocal(global: Event): Event = Event(invGeoTransform.transformPoint(global.space),global.t)
}