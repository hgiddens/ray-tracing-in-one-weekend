use crate::colour::Albedo;
use crate::hitable::HitRecord;
use crate::ray::Ray;
use crate::vec3::Vec3;

use rand::Rng;

pub struct Scatter {
    pub ray: Ray,
    pub attenuation: Albedo,
}

pub trait Material {
    fn scatter(&self, ray: &Ray, hit: &HitRecord) -> Option<Scatter>;
}

pub struct Lambertian {
    pub albedo: Albedo
}

// TODO: This used to take the RNG as an argument, it'd be nice if it still did(?)
fn random_in_unit_sphere() -> Vec3 {
    loop {
        let p = Vec3 {
            x: (2.0 * rand::thread_rng().gen::<f32>()) - 1.0,
            y: (2.0 * rand::thread_rng().gen::<f32>()) - 1.0,
            z: (2.0 * rand::thread_rng().gen::<f32>()) - 1.0,
        };
        let squared_length = p.x * p.x + p.y * p.y + p.z * p.z;
        if squared_length < 1.0 {
            return p;
        }
    }
}

impl Material for Lambertian {
    fn scatter(&self, _ray: &Ray, hit: &HitRecord) -> Option<Scatter> {
        let target = hit.p + hit.normal + random_in_unit_sphere();
        Some(Scatter { ray: Ray::new(hit.p, target - hit.p), attenuation: self.albedo })
    }
}

fn reflect(v: Vec3, n: Vec3) -> Vec3 {
    v - (2.0 * v.dot(n) * n)
}

pub struct Metal {
    pub albedo: Albedo,
    pub fuzz: f32,
}

impl Material for Metal {
    fn scatter(&self, ray: &Ray, hit: &HitRecord) -> Option<Scatter> {
        let reflected = reflect(ray.direction().unit_vector(), hit.normal);
        let scattered = Ray::new(hit.p, reflected + (self.fuzz * random_in_unit_sphere()));
        if scattered.direction().dot(hit.normal) > 0.0 {
            Some(Scatter { ray: scattered, attenuation: self.albedo })
        } else {
            None
        }
    }
}
