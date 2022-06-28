use crate::material::Material;
use crate::pt3::Pt3;
use crate::ray::Ray;
use crate::scene_object::{HitRecord, SceneObject};

pub struct Sphere<'a> {
    centre: Pt3,
    radius: f32,
    material: Box<dyn 'a + Material>,
}

impl<'a> Sphere<'a> {
    pub fn new<T: 'a + Material>(centre: Pt3, radius: f32, material: T) -> Self {
        Sphere {
            centre,
            radius,
            material: Box::new(material),
        }
    }
}

impl<'a> SceneObject<'a> for &'a Sphere<'a> {
    fn hit(self, ray: &Ray, t_min: f32, t_max: f32) -> Option<HitRecord<'a>> {
        let oc = ray.origin - self.centre;
        let a = ray.direction.dot(ray.direction);
        let b = oc.dot(ray.direction);
        let c = oc.dot(oc) - self.radius * self.radius;
        let discriminant = b * b - a * c;

        let try_intersection = |t: f32| {
            if t < t_max && t > t_min {
                let p = ray.point_at_parameter(t);
                Some(HitRecord {
                    t,
                    p,
                    normal: (p - self.centre) / self.radius,
                    material: &*self.material,
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
