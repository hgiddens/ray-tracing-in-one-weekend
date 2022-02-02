mod colour;
mod pt3;
mod ray;
mod vec3;

use crate::colour::Colour;
use crate::pt3::Pt3;
use crate::ray::Ray;
use crate::vec3::Vec3;

fn ray_colour(ray: Ray) -> Colour {
    let unit_direction = ray.direction().unit_vector();
    let t = (unit_direction.y + 1.0) * 0.5;
    Colour::blend(Colour::white(), Colour::light_sky_blue(), t)
}

fn main() {
    let cols = 200;
    let rows = 100;
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

    for row in (0..rows).rev() {
        for col in 0..cols {
            let u = col as f32 / cols as f32;
            let v = row as f32 / rows as f32;
            let ray = Ray::from_origin_to(lower_left_corner + u * horizontal + v * vertical);
            let colour = ray_colour(ray);
            let (ir, ig, ib) = colour.rgb8();
            println!("{} {} {}", ir, ig, ib);
        }
    }
}
