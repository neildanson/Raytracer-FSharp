namespace Raytracer
open System

type Vector3D(x:float, y : float, z : float) =
    member vector.X = x
    member vector.Y = y
    member vector.Z = z
    //Addition
    static member (+) (v1:Vector3D, v2:Vector3D) = 
        Vector3D(v1.X + v2.X, v1.Y + v2.Y,v1.Z + v2.Z)

    //Subtraction
    static member (-) (v1:Vector3D, v2:Vector3D) = 
        Vector3D(v1.X - v2.X, v1.Y - v2.Y,v1.Z - v2.Z)

    //Scalar
    static member (*) (v1:Vector3D, l) =
        Vector3D(v1.X * l, v1.Y * l,v1.Z * l)

    static member (/) (v1:Vector3D, l) =
        Vector3D(v1.X / l, v1.Y / l,v1.Z / l)

    //Component
    static member (*) (v1:Vector3D, v2:Vector3D) =
        Vector3D(v1.X * v2.X, v1.Y * v2.Y,v1.Z * v2.Z)    
    
    //Cross product |a||b|sin(theta)
    static member Cross(v1:Vector3D, v2:Vector3D) = 
        Vector3D(v1.Y * v2.Z - v1.Z * v2.Y,
                 v1.Z * v2.X - v1.X * v2.Z,
                 v1.X * v2.Y - v1.Y * v2.X)

    //Dot product |a||b|cos(theta)
    static member Dot(v1:Vector3D, v2:Vector3D) = 
        (v1.X * v2.X) + (v1.Y * v2.Y) + (v1.Z * v2.Z)
    
    //Normalize the vector
    static member Normalize(v) =
        let mag = Math.Sqrt(Vector3D.Dot(v, v))
        let div = if mag = 0.0 then System.Double.PositiveInfinity else 1.0 /mag
        v * div 

    static member Mag(v) = 
        Math.Sqrt(Vector3D.Dot(v, v))

type Color(r:float, g:float, b:float) =
    let legalize (d : float) = if d > 1.0 then 1.0 else d * 255.0
    static let zero = Color.RGB(0.0,0.0,0.0)
    member __.R = r
    member __.G = g
    member __.B = b
    static member (+) (a:Color , b:Color) = 
        Color(a.R + b.R, a.G + b.G, a.B + b.B)

    static member (-) (a:Color , b:Color) = 
        Color(a.R - b.R, a.G - b.G, a.B - b.B)

    static member (*) (a:Color , b:Color) = 
        Color(a.R * b.R, a.G * b.G, a.B * b.B)

    static member (*) (a:Color , l) = 
        Color(a.R * l, a.G * l, a.B * l)
    
    static member (/) (a:Color , l) = 
        Color(a.R / l, a.G / l, a.B / l)
    
    static member get_Zero() = zero

    static member RGB(r, g, b) = 
        Color(r,g,b)

    member this.ToColor() = 
        let r,g,b = (int (legalize this.R), int (legalize this.G), int (legalize this.B))
        let a = 255 <<< 24
        let r = r<<<16
        let g = g<<<8
        let b = b
        a+r+g+b

type Ray(position : Vector3D, direction:Vector3D) = 
    member ray.Position = position 
    member ray.Direction = direction
    
type Light(pos : Vector3D, col : Color) = 
    member light.Position = pos
    member light.Color = col

type Camera(position:Vector3D, lookAt:Vector3D) = 
    let forward = Vector3D.Normalize(lookAt- position)
    let down = Vector3D(0.0,-1.0, 0.0)
    let right = Vector3D.Normalize(Vector3D.Cross (forward, down))* 1.5
    let up = Vector3D.Normalize(Vector3D.Cross (forward, right)) * 1.5
    member cam.Position = position
    member cam.Forward = forward
    member cam.Up = up
    member cam.Right = right

type Intersection(ray:Ray, dist:float) = 
    member isect.Ray = ray
    member isect.Dist = dist

type Surface =
     abstract member Diffuse : Vector3D -> Color;
     abstract member Specular : Vector3D -> Color;
     abstract member Reflect : Vector3D -> float;
     abstract member Roughness : float
    
