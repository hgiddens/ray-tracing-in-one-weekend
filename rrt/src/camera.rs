use crate::pt3::Pt3;
use crate::ray::Ray;
use crate::vec3::Vec3;

pub trait Camera {
    fn get_ray(&self, u: f32, v: f32) -> Ray;
}

#[derive(Debug)]
pub struct SimpleCamera {
    origin: Pt3,
    lower_left_corner: Pt3,
    horizontal: Vec3,
    vertical: Vec3,
}

impl SimpleCamera {
    // vfov is top to bottom in degrees
    pub fn fov(look_from: Pt3, look_at: Pt3, vup: Vec3, vfov: f32, aspect: f32) -> Self {
        let theta = vfov * std::f32::consts::PI / 180.0;
        let half_height = (theta / 2.0).tan();
        let half_width = aspect * half_height;

        let w = (look_from - look_at).unit_vector();
        let u = vup.cross(w).unit_vector();
        let v = w.cross(u);

        SimpleCamera {
            origin: look_from,
            lower_left_corner: look_from - (half_width * u) - (half_height * v) - w,
            horizontal: 2.0 * half_width * u,
            vertical: 2.0 * half_height * v,
        }
    }
}

impl Camera for SimpleCamera {
    fn get_ray(&self, u: f32, v: f32) -> Ray {
        let direction = self.lower_left_corner + u * self.horizontal + v * self.vertical;
        Ray {
            origin: self.origin,
            direction: direction - self.origin,
        }
    }
}
