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
    pub fn new(origin: Pt3, lower_left_corner: Pt3, horizontal: Vec3, vertical: Vec3) -> Self {
        SimpleCamera {
            origin,
            lower_left_corner,
            horizontal,
            vertical,
        }
    }
}

impl Camera for SimpleCamera {
    fn get_ray(&self, u: f32, v: f32) -> Ray {
        let direction = self.lower_left_corner + u * self.horizontal + v * self.vertical;
        Ray::new(self.origin, direction - self.origin)
    }
}
