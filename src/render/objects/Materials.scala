package render.objects

import render.geomeries.GeoInfo
import render.maths._
import render.objects.Spectrum.{SpecFunc, Func1}

import scala.util.Random

object Materials

abstract class Mat{
  def shade(localRay: Ray, checkResult: HitCheckResult, info: GeoInfo, scene: RelaScene, sample: RandomSample, simplify: Int, instance: HitInstance): RadianceSet
  
  def getLightDis(instance: HitInstance,checkResult: HitCheckResult)= (instance.globalToLocal(checkResult.globalEvent).t - checkResult.localEvent.t)*RadianceSet.k
}

object AngleFunctions{
  type AngleFunction=(Ray,GeoInfo)=>Float

  def lambiant(cosPower:Double):AngleFunction=(ray,info)=>{
    val cos= -(ray.direction dot info.normal)
    if(cos<0) 0f
    else math.pow(cos,cosPower).toFloat
  }

  val simpleDiffuse:AngleFunction=(ray,info)=>1
}
object ColorFunctions{
  type ColorFunc=(RadianceSet,GeoInfo)=>RadianceSet

  def getAbsort(radiance: Radiance):Float= (RadianceSet.color dot radiance).componentSum/radiance.componentSum

  def pureColorSingleSide(color:Radiance,absort:Float):ColorFunc=(r,info)=>{
    if(info.isOut) RadianceSet(r.radiance dot color,r.complex*absort)
    else r
  }
  
  def pureColorSingleSide(color:Radiance):ColorFunc=pureColorSingleSide(color,getAbsort(color))

  def checkerPattern(rad1:Radiance,rad2:Radiance,uSize:Double,vSize:Double):ColorFunc={
    val f1=pureColorSingleSide(rad1,getAbsort(rad1))
    val f2=pureColorSingleSide(rad2,getAbsort(rad2))
    checkerPattern(f1,f2,uSize,vSize)
  }

  def checkerPattern(func1:ColorFunc,func2:ColorFunc,uSize:Double,vSize:Double):ColorFunc=(r,info)=>{
    val uvPoint=info.uvPoint

    val nU=(uvPoint.x/uSize).floor.toInt
    val nV=(uvPoint.y/vSize).floor.toInt
    if((nU+nV)%2==0) func1(r,info)
    else func2(r,info)
  }
}

import ColorFunctions.ColorFunc
import AngleFunctions.AngleFunction

class DiffuseMat(colorFunc:ColorFunc,angleFunc:AngleFunction,sampleNum:Int,sampleSimplify:Int) extends Mat{
  override def shade(localRay: Ray, checkResult: HitCheckResult, info: GeoInfo, scene: RelaScene, sample: RandomSample, simplify: Int, instance: HitInstance): RadianceSet = {
    val frame=VectorFrame.fromNormal(info.normal)
    val sampleToUse = sampleNum/simplify
    if(sampleToUse==0) return RadianceSet(Radiance.zero,Vector2.zero)

    var radianceSum=Radiance.zero
    var complexSum=Vector2.zero
    (0 until sampleToUse).map(i=>{
      val v=sample.nextSampleOnHemisphere()
      def cos=if(v.y>0) v.y.toFloat else 0f

      val direc=frame.localToGlobal(v)
      val globalRay=Ray(checkResult.globalEvent,direc,localRay.maxRecursive-1)
      val radianceSet=scene.traceRay(globalRay, sample, sampleSimplify*simplify).radianceSet
      radianceSum+=radianceSet.radiance*cos
      val dis=getLightDis(instance,checkResult)
      complexSum += radianceSet.complex.rotate(dis)*cos
    })
    
    val set=RadianceSet((radianceSum+RadianceSet.color*complexSum.length.toFloat)/sampleToUse ,Vector2.zero)
    colorFunc(set,info)
  }
}

class PureColor(radianceSet: RadianceSet) extends Mat{
  override def shade(localRay: Ray, checkResult: HitCheckResult, info: GeoInfo, scene: RelaScene, sample: RandomSample, simplify: Int, instance: HitInstance): RadianceSet = radianceSet
}

class Reflective(colorFunc: ColorFunc) extends Mat{
  override def shade(localRay: Ray, checkResult: HitCheckResult, info: GeoInfo, scene: RelaScene, sample: RandomSample, simplify: Int, instance: HitInstance): RadianceSet = {
    val incident=localRay.direction
    val normal: Vector3 = info.normal
    val delta= normal*(-2*(incident dot normal))
    val reflected=incident+ delta
    val newRay=Ray(checkResult.globalEvent,reflected,localRay.maxRecursive-1)
    val traced=scene.traceRay(newRay, sample, simplify).radianceSet
    
    val set=RadianceSet(traced.radiance,traced.complex.rotate(getLightDis(instance,checkResult)))
    
    colorFunc(set,info)
  }
}

class Refractive(iof:Double,reflectance:Double,reflectColor: ColorFunc,colorFilter: Radiance,absort:Double) extends Mat{

  val invIof=1.0/iof

  def filter(dis:Double,r:RadianceSet):RadianceSet={
    def pow(f:Double)= math.exp(-dis*f).toFloat
    val filt=Radiance(pow(colorFilter.r),pow(colorFilter.g),pow(colorFilter.b))
    RadianceSet(filt dot r.radiance,r.complex*pow(absort))
  }
  override def shade(localRay: Ray, checkResult: HitCheckResult, info: GeoInfo, scene: RelaScene, sample: RandomSample, simplify: Int, instance: HitInstance): RadianceSet = {
    def filterResult(result: TraceResult,needFilter:Boolean):RadianceSet={
      if(needFilter) {
        val dis=instance.globalToLocal(result.globalEvent).t-checkResult.localEvent.t
        filter(dis,result.radianceSet)
      }
      else result.radianceSet
    }

    val incident=localRay.direction
    val normal: Vector3 = info.normal
    val cos= -(incident dot normal)

    val delta = normal * (2 * cos)
    val reflected = incident + delta
    val newDepth = localRay.maxRecursive - 1
    val reflectRay = Ray(checkResult.globalEvent, reflected, newDepth)
    val reflectResult: TraceResult = scene.traceRay(reflectRay, sample, simplify)
    val reflectRadiance = filterResult(reflectResult,! info.isOut)
    val effectiveIof=if(info.isOut) invIof else iof
    val sin=math.sqrt(1-cos*cos)
    val sin2=sin*effectiveIof
    if(sin2>1){
      //Totally reflective
      reflectRadiance
    }else {
      val tan2 = sin2 / math.sqrt(1 - sin2 * sin2)
      val parallel = (incident + normal * cos).normalized
      val refracted = (parallel * tan2 - normal).normalized
      val refractRay = Ray(checkResult.globalEvent, refracted, newDepth)

      val f = (reflectance + (1 - reflectance) * math.pow(1 - cos, 5)).toFloat

      val refractResult = scene.traceRay(refractRay, sample, simplify)
      val refractRadiance=filterResult(refractResult,info.isOut)

      reflectColor(reflectRadiance,info) * f + refractRadiance * (1 - f)
    }
  }
}

object Spectrum{
  type Func1=Double=>Double
  type SpecFunc=Double=>Radiance

  def linearFunc(from:Double,to:Double):Func1=lamda=>from+(to-from)*lamda
  def linearSpectrum:SpecFunc=lamda=>{
    val r=1f-lamda.toFloat
    val b=lamda.toFloat
    val g=1f-2f*math.abs(lamda-0.5).toFloat
    Radiance(r,g,b)*3f
  }
}

/**
 * Caution: this material is not for monocolour light. Use Refractive mat instead.
 * @param iofFunc
 * @param filterFunc
 * @param spectrum
 * @param reflectance
 * @param reflectColor
 * @param colorSample
 */
class Colorful(iofFunc:Func1,filterFunc:Func1,spectrum:SpecFunc,reflectance:Double,reflectColor: ColorFunc,colorSample:Int) extends Mat{
  //TODO : Change the mechanism to detect inside-object ray
  val random=new Random()
  override def shade(localRay: Ray, checkResult: HitCheckResult, info: GeoInfo, scene: RelaScene, sample: RandomSample, simplify: Int, instance: HitInstance): RadianceSet = {
    def filterStrength(globalEvent:Event,p:Double,needFilter:Boolean):Float= {
      if(needFilter) {
        val dis = instance.globalToLocal(globalEvent).t - checkResult.localEvent.t
        val pow = math.exp(-dis * p)
        pow.toFloat
      }else{
        1f
      }
    }

    val incident=localRay.direction
    val normal: Vector3 = info.normal
    val cos= -(incident dot normal)

    val delta = normal * (2 * cos)
    val reflected = incident + delta
    val newDepth = localRay.maxRecursive - 1
    val reflectRay = Ray(checkResult.globalEvent, reflected, newDepth)
    val reflectResult: TraceResult = scene.traceRay(reflectRay, sample, simplify)
    val reflectSet = reflectColor(reflectResult.radianceSet, info)

    val effectSample=math.max(colorSample/simplify,1)
    val colors=for(i<-0 until effectSample) yield {
      val lambda=random.nextDouble()
      val iof=iofFunc(lambda)
      val color=spectrum(lambda)
      val effectiveIof = if (info.isOut) 1.0/iof else iof
      val sin = math.sqrt(1 - cos * cos)
      val sin2 = sin * effectiveIof
      val reflectFiltered=(reflectSet.radiance dot color)*filterStrength(reflectResult.globalEvent,filterFunc(lambda),!info.isOut)
      if (sin2 > 1) {
        //Totally reflective
        reflectFiltered
      } else {
        val tan2 = sin2 / math.sqrt(1 - sin2 * sin2)
        val parallel = (incident + normal * cos).normalized
        val refracted = (parallel * tan2 - normal).normalized
        val refractRay = Ray(checkResult.globalEvent, refracted, newDepth)

        val f = (reflectance + (1 - reflectance) * math.pow(1 - cos, 5)).toFloat

        val refractResult = scene.traceRay(refractRay, sample, simplify*effectSample)
        val refractFiltered = refractResult.radianceSet.radiance*filterStrength(refractResult.globalEvent,filterFunc(lambda),info.isOut)

        reflectFiltered * f + refractFiltered.dot(color) * (1 - f)
      }
    }
    RadianceSet(colors.fold(Radiance.zero)((a,b)=>a+b)/effectSample,Vector2.zero)
  }
}

class MixMat(components: List[(Mat,Float)]) extends Mat{
  override def shade(localRay: Ray, checkResult: HitCheckResult, info: GeoInfo, scene: RelaScene,
                     sample: RandomSample, simplify: Int, instance: HitInstance): RadianceSet = {
    components.map(pair=>pair._1.shade(localRay,checkResult,info,scene,sample,simplify,instance)*pair._2).
      fold(RadianceSet.zero)(_+_)
  }
}