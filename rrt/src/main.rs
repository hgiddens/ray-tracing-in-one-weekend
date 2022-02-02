mod colour;
mod hitable;
mod pt3;
mod ray;
mod sphere;
mod vec3;

use crate::colour::Colour;
use crate::hitable::Hitable;
use crate::pt3::Pt3;
use crate::ray::Ray;
use crate::sphere::Sphere;
use crate::vec3::Vec3;

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

    for row in (0..rows).rev() {
        for col in 0..cols {
            let u = col as f32 / cols as f32;
            let v = row as f32 / rows as f32;
            let ray = Ray::from_origin_to(lower_left_corner + u * horizontal + v * vertical);
            let colour = ray_colour(&scene, ray);
            let (ir, ig, ib) = colour.rgb8();
            println!("{} {} {}", ir, ig, ib);
        }
    }
}
