namespace Raytracer
open System

[<AbstractClass>]
type SceneObject (objectSurface : Surface) =
    member this.Surface = objectSurface 
    abstract Intersects : Ray -> Intersection option
    abstract Normal : Vector3D -> Vector3D

type Sphere(center : Vector3D, radius : float, surface : Surface) = 
    inherit SceneObject(surface)
    member this.Center = center
    member this.Radius = radius

    override this.Intersects (ray : Ray) = 
        let eo = this.Center - ray.Position
        let v = Vector3D.Dot(eo, ray.Direction)
        let getDist = 
            if v < 0.0 then 0.0
            else
                let disc = Math.Pow(radius, 2.0) - (Vector3D.Dot(eo, eo) - Math.Pow(v, 2.0))
                if disc < 0.0 then 0.0 else v - Math.Sqrt(disc)
        let dist = getDist 
        if dist = 0.0 then None else Some(Intersection(ray, dist))

    override this.Normal pos = 
        Vector3D.Normalize(pos- this.Center)

type Plane (normal : Vector3D, offset : float, surface : Surface) = 
    inherit SceneObject(surface)
    member this.Offset = offset
    member this.PlaneNormal = normal
    override this.Intersects ray = 
        let denom = Vector3D.Dot(this.PlaneNormal, ray.Direction)
        if denom > 0.0 then None else Some(Intersection(ray,  (Vector3D.Dot(this.PlaneNormal, ray.Position) + this.Offset) / denom ))
            
    override this.Normal pos = 
        this.PlaneNormal