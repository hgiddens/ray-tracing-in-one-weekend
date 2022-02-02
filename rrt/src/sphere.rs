use crate::hitable::{HitRecord, Hitable};
use crate::pt3::Pt3;
use crate::ray::Ray;

pub struct Sphere {
    centre: Pt3,
    radius: f32,
}

impl Sphere {
    pub fn new(centre: Pt3, radius: f32) -> Self {
        Sphere { centre, radius }
    }
}

impl Hitable for Sphere {
    fn hit(&self, ray: &Ray, t_min: f32, t_max: f32) -> Option<HitRecord> {
        let oc = ray.origin() - self.centre;
        let a = ray.direction().dot(ray.direction());
        let b = oc.dot(ray.direction());
        let c = oc.dot(oc) - self.radius * self.radius;
        let discriminant = b * b - a * c;

        let try_intersection = |t: f32| {
            if t < t_max && t > t_min {
                let p = ray.point_at_parameter(t);
                Some(HitRecord {
                    t,
                    p,
                    normal: (p - self.centre) / self.radius,
                })
            } else {
                None
            }
        };

        if discriminant > 0.0 {
            if let Some(t) = try_intersection((-b - (b * b - a * c).sqrt()) / a) {
                return Some(t);
            }
            if let Some(t) = try_intersection((-b + (b * b - a * c).sqrt()) / a) {
                return Some(t);
            }
        }

        None
    }
}
