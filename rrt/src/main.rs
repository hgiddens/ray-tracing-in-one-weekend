mod camera;
mod colour;
mod hitable;
mod material;
mod pt3;
mod ray;
mod sphere;
mod vec3;

use crate::camera::{Camera, SimpleCamera};
use crate::colour::{Albedo, Colour};
use crate::hitable::Hitable;
use crate::material::{Lambertian, Metal};
use crate::pt3::Pt3;
use crate::ray::Ray;
use crate::sphere::Sphere;
use crate::vec3::Vec3;

use rand::Rng;

const SHADOW_ACNE_MIN: f32 = 0.001;
const DEPTH_MAX: i32 = 50;

fn ray_colour<T: Hitable>(scene: &T, ray: Ray, depth: i32) -> Colour {
    if let Some(record) = scene.hit(&ray, SHADOW_ACNE_MIN, f32::MAX) {
        // TODO: Some way to do if x and let(y) = z { ... } ?
        if depth < DEPTH_MAX {
            if let Some(scatter) = record.material.scatter(&ray, &record) {
                ray_colour(scene, scatter.ray, depth + 1) * scatter.attenuation
            } else { Colour::black() }
        } else { Colour::black() }
    } else {
        let unit_direction = ray.direction().unit_vector();
        let t = (unit_direction.y + 1.0) * 0.5;
        Colour::blend(Colour::white(), Colour::light_sky_blue(), t)
    }
}

fn main() {
    let cols = 800;
    let rows = 400;
    let samples = 100;

    println!("P3");
    println!("{} {}", cols, rows);
    println!("255");

    let lower_left_corner = Pt3 {
        x: -2.0,
        y: -1.0,
        z: -1.0,
    };
    let horizontal = Vec3::new(4.0, 0.0, 0.0);
    let vertical = Vec3::new(0.0, 2.0, 0.0);
    let scene = vec![
        Sphere::new(
            Pt3 { x: 0.0, y: 0.0, z: -1.0, }, 0.5, Lambertian { albedo: Albedo { r: 0.8, g: 0.3, b: 0.3, } }
        ),
        Sphere::new(
            Pt3 { x: 0.0, y: -100.5, z: -1.0 }, 100.0, Lambertian { albedo: Albedo { r: 0.8, g: 0.8, b: 0.0, } }
        ),
        Sphere::new(
            Pt3 { x: 1.0, y: 0.0, z: -1.0, }, 0.5, Metal { albedo: Albedo { r: 0.8, g: 0.6, b: 0.2, }, fuzz: 1.0, }
        ),
        Sphere::new(
            Pt3 { x: -1.0, y: 0.0, z: -1.0 }, 0.5, Metal { albedo: Albedo { r: 0.8, g: 0.8, b: 0.8, }, fuzz: 0.3, }
        ),
    ];

    let origin = Pt3 {
        x: 0.0,
        y: 0.0,
        z: 0.0,
    };
    let camera = SimpleCamera::new(origin, lower_left_corner, horizontal, vertical);
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
