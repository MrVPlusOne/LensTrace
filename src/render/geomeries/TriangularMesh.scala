package render.geomeries

import render.maths.Vector3

/**
 * Created by weijiayi on 5/19/15.
 */
class TriangularMesh(triangles:Seq[Triangle]) extends Geometry{
  override def geoInfo(hitPoint: Vector3, isOutside: Boolean, customData: Any): GeoInfo = ???

  override def checkHit(origin: Vector3, dir: Vector3): HitCheck = {
    val hits = triangles.map(_.checkHit(origin,dir)).filter(_.isInstanceOf[Hit])
    if(hits.isEmpty) Missed
    else hits.map(_.asInstanceOf[Hit]).minBy(_.distance)
  }
}
