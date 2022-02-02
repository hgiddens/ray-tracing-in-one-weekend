use crate::colour::Colour;
use crate::pt3::Pt3;
use crate::ray::Ray;
use crate::vec3::Vec3;

pub struct Sphere {
    centre: Pt3,
    radius: f32,
}

impl Sphere {
    pub fn new(centre: Pt3, radius: f32) -> Self {
        Sphere { centre, radius }
    }

    pub fn hit(&self, ray: Ray) -> Option<Colour> {
        let oc: Vec3 = ray.origin() - self.centre;
        let a: f32 = ray.direction().dot(ray.direction());
        let b: f32 = 2.0 * oc.dot(ray.direction());
        let c: f32 = oc.dot(oc) - self.radius * self.radius;

        let discriminant: f32 = b * b - 4.0 * a * c;
        if discriminant > 0.0 {
            Some(Colour::red())
        } else {
            None
        }
    }
}
