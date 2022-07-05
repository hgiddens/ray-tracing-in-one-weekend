mod camera;
mod colour;
mod material;
mod pt3;
mod ray;
mod scene_object;
mod sphere;
mod vec3;

use crate::camera::{Camera, SimpleCamera};
use crate::colour::{Albedo, Colour};
use crate::material::{Dielectric, Lambertian, Metal};
use crate::pt3::Pt3;
use crate::ray::Ray;
use crate::scene_object::{HitRecord, SceneObject};
use crate::sphere::Sphere;
use crate::vec3::Vec3;

use rand::Rng;

const SHADOW_ACNE_MIN: f32 = 0.001;
const DEPTH_MAX: i32 = 50;

fn ray_colour(scene: &[Sphere], ray: Ray, depth: i32) -> Colour {
    let mut earliest_hit: Option<HitRecord> = None;
    for sphere in scene {
        earliest_hit = match earliest_hit {
            None => sphere.hit(&ray, SHADOW_ACNE_MIN, f32::MAX),
            Some(ref hit) => sphere.hit(&ray, SHADOW_ACNE_MIN, hit.t).or(earliest_hit),
        }
    }

    // TODO: so we do a bunch of work and then throw it away if we got too
    // deep? that's stupid? but I guess we want to see if we bounce away into
    // nothingness or if we hit the skybox.
    match earliest_hit {
        // TODO: This seems gross.
        Some(hit) => hit
            .material
            .scatter(&ray, &hit)
            .filter(|_| depth < DEPTH_MAX)
            .map_or(Colour::black(), |scatter| {
                ray_colour(scene, scatter.ray, depth + 1) * scatter.attenuation
            }),
        _ => {
            let unit_direction = ray.direction.unit_vector();
            let t = (unit_direction.y + 1.0) * 0.5;
            Colour::blend(Colour::white(), Colour::light_sky_blue(), t)
        }
    }
}

fn main() {
    let cols = 800;
    let rows = 400;
    let samples = 100;

    println!("P3");
    println!("{} {}", cols, rows);
    println!("255");

    let scene = vec![
        Sphere::new(
            Pt3 {
                x: 0.0,
                y: 0.0,
                z: -1.0,
            },
            0.5,
            Lambertian {
                albedo: Albedo {
                    r: 0.1,
                    g: 0.2,
                    b: 0.5,
                },
            },
        ),
        Sphere::new(
            Pt3 {
                x: 0.0,
                y: -100.5,
                z: -1.0,
            },
            100.0,
            Lambertian {
                albedo: Albedo {
                    r: 0.8,
                    g: 0.8,
                    b: 0.0,
                },
            },
        ),
        Sphere::new(
            Pt3 {
                x: 1.0,
                y: 0.0,
                z: -1.0,
            },
            0.5,
            Metal {
                albedo: Albedo {
                    r: 0.8,
                    g: 0.6,
                    b: 0.2,
                },
                fuzz: 0.3,
            },
        ),
        Sphere::new(
            Pt3 {
                x: -1.0,
                y: 0.0,
                z: -1.0,
            },
            0.5,
            Dielectric { ref_idx: 1.5 },
        ),
        Sphere::new(
            Pt3 {
                x: -1.0,
                y: 0.0,
                z: -1.0,
            },
            -0.45,
            Dielectric { ref_idx: 1.5 },
        ),
    ];

    let camera = SimpleCamera::fov(
        Pt3 {
            x: -2.0,
            y: 2.0,
            z: 1.0,
        },
        Pt3 {
            x: 0.0,
            y: 0.0,
            z: -1.0,
        },
        Vec3 {
            x: 0.0,
            y: 1.0,
            z: 0.0,
        },
        20.0,
        (cols as f32) / (rows as f32),
    );
    let mut rng = rand::thread_rng();

    for row in (0..rows).rev() {
        for col in 0..cols {
            let mut colour = Colour::new(0.0, 0.0, 0.0);
            // TODO: Extract supersampler abstraction.
            for _ in 0..samples {
                let du: f32 = rng.gen();
                let dv: f32 = rng.gen();
                let u = (col as f32 + du) / cols as f32;
                let v = (row as f32 + dv) / rows as f32;
                let ray = camera.get_ray(u, v);
                colour = colour + ray_colour(&scene, ray, 0);
            }
            // TODO: Colour blending function rather than math on colours.
            colour = colour / samples;
            let (ir, ig, ib) = colour.gamma2().rgb8();
            println!("{} {} {}", ir, ig, ib);
        }
    }
}
