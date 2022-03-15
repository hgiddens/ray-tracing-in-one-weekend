mod camera;
mod colour;
mod hitable;
mod pt3;
mod ray;
mod sphere;
mod vec3;

use crate::camera::{Camera, SimpleCamera};
use crate::colour::Colour;
use crate::hitable::Hitable;
use crate::pt3::Pt3;
use crate::ray::Ray;
use crate::sphere::Sphere;
use crate::vec3::Vec3;

use rand::Rng;

fn ray_colour<T: Hitable>(scene: &T, ray: Ray) -> Colour {
    if let Some(record) = scene.hit(&ray, 0.0, f32::MAX) {
        return Colour::new(
            0.5 * (record.normal.x + 1.0),
            0.5 * (record.normal.y + 1.0),
            0.5 * (record.normal.z + 1.0),
        );
    }

    let unit_direction = ray.direction().unit_vector();
    let t = (unit_direction.y + 1.0) * 0.5;
    Colour::blend(Colour::white(), Colour::light_sky_blue(), t)
}

fn main() {
    let cols = 800;
    let rows = 400;
    let samples = 10;

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
            Pt3 {
                x: 0.0,
                y: 0.0,
                z: -1.0,
            },
            0.5,
        ),
        Sphere::new(
            Pt3 {
                x: 0.0,
                y: -100.5,
                z: -1.0,
            },
            100.0,
        ),
    ];

    let origin = Pt3 {
        x: 0.0,
        y: 0.0,
        z: 0.0,
    };
    let camera = SimpleCamera::new(origin, lower_left_corner, horizontal, vertical);
    let mut rand_rng = rand::thread_rng();

    for row in (0..rows).rev() {
        for col in 0..cols {
            let mut colour = Colour::new(0.0, 0.0, 0.0);
            // TODO: Extract supersampler abstraction.
            for _ in 0..samples {
                let du: f32 = rand_rng.gen();
                let dv: f32 = rand_rng.gen();
                let u = (col as f32 + du) / cols as f32;
                let v = (row as f32 + dv) / rows as f32;
                let ray = camera.get_ray(u, v);
                colour = colour + ray_colour(&scene, ray);
            }
            // TODO: Colour blending function rather than math on colours.
            colour = colour / samples;
            let (ir, ig, ib) = colour.rgb8();
            println!("{} {} {}", ir, ig, ib);
        }
    }
}
