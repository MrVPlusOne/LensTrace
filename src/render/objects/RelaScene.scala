package render.objects

import render.maths.{RandomSample, Vector3, Event, Ray}

case class TraceResult(radianceSet: RadianceSet, globalEvent:Event)

class RelaScene (
            val instances:List[HitInstance],
            val lights:List[Light],
            val background:RadianceSet
             ){



  private def backRadiance(ray: Ray): RadianceSet =
    lights.map(l=>l.radianceSetAt(ray)).fold(RadianceSet.zero)((r1,r2)=>r1+r2)+background

  def traceRay(ray: Ray, matSampler: RandomSample, simplify: Int=1):TraceResult= {
    def getRadianceSet(instance: HitInstance, hitCheck: HitCheckResult): RadianceSet = {
      instance.getRadianceSet(ray, this, hitCheck, matSampler, simplify)
    }

    def loop(left: List[HitInstance], nearest: Option[HitInstance], minResult: HitCheckResult): (Option[HitInstance], HitCheckResult) = left match {
      case Nil => (nearest, minResult)
      case head :: tail => head.hitCheck(ray) match {
        case Some(result) if result.globalEvent.t < minResult.globalEvent.t => loop(tail, Some(head), result)
        case _ => loop(tail, nearest, minResult)
      }
    }


    if(ray.maxRecursive<0)
      TraceResult(background,Event.positiveInf)
    else {
      val (hited, checkResult) = loop(instances, None,
        HitCheckResult(Event.origin, Event(Vector3.zero, Double.PositiveInfinity), true,null))

      hited match {
        case None => TraceResult(backRadiance(ray),Event.positiveInf)
        case Some(instance) => {
          TraceResult(getRadianceSet(instance, checkResult),checkResult.globalEvent)
        }
      }
    }
  }

}
