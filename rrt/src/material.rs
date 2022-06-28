use crate::colour::Albedo;
use crate::ray::Ray;
use crate::scene_object::HitRecord;
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
    pub albedo: Albedo,
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
        Some(Scatter {
            ray: Ray {
                origin: hit.p,
                direction: target - hit.p,
            },
            attenuation: self.albedo,
        })
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
        let reflected = reflect(ray.direction.unit_vector(), hit.normal);
        let scattered = Ray {
            origin: hit.p,
            direction: reflected + (self.fuzz * random_in_unit_sphere()),
        };
        if scattered.direction.dot(hit.normal) > 0.0 {
            Some(Scatter {
                ray: scattered,
                attenuation: self.albedo,
            })
        } else {
            None
        }
    }
}

fn refract(v: Vec3, n: Vec3, ni_over_nt: f32) -> Option<Vec3> {
    let uv = v.unit_vector();
    let dt = uv.dot(n);
    let discriminant = 1.0 - ni_over_nt * ni_over_nt * (1.0 - dt * dt);
    if discriminant > 0.0 {
        Some(ni_over_nt * (uv - n * dt) - n * discriminant.sqrt())
    } else {
        None
    }
}

fn schlick(cosine: f32, ref_idx: f32) -> f32 {
    let r0 = (1.0 - ref_idx) / (1.0 + ref_idx);
    let r0 = r0 * r0;
    r0 + (1.0 - r0) * (1.0 - cosine).powf(5.0)
}

pub struct Dielectric {
    pub ref_idx: f32,
}

impl Material for Dielectric {
    fn scatter(&self, ray: &Ray, hit: &HitRecord) -> Option<Scatter> {
        let attenuation = Albedo {
            r: 1.0,
            g: 1.0,
            b: 1.0,
        };
        let ray_dot_n = ray.direction.dot(hit.normal);
        let (outward_normal, ni_over_nt, cosine) = if ray_dot_n > 0.0 {
            (
                -hit.normal,
                self.ref_idx,
                self.ref_idx * ray_dot_n / ray.direction.length(),
            )
        } else {
            (
                hit.normal,
                1.0 / self.ref_idx,
                -ray_dot_n / ray.direction.length(),
            )
        };

        if let Some(refracted) = refract(ray.direction, outward_normal, ni_over_nt) {
            let reflect_prob = schlick(cosine, self.ref_idx);
            if rand::thread_rng().gen::<f32>() >= reflect_prob {
                return Some(Scatter {
                    ray: Ray {
                        origin: hit.p,
                        direction: refracted,
                    },
                    attenuation,
                });
            }
        }

        let reflected = reflect(ray.direction, hit.normal);
        Some(Scatter {
            ray: Ray {
                origin: hit.p,
                direction: reflected,
            },
            attenuation,
        })
    }
}
