use crate::pt3::Pt3;
use crate::vec3::Vec3;

#[derive(Clone, Copy, Debug)]
pub struct Ray {
    origin: Pt3,
    direction: Vec3,
}

impl Ray {
    pub fn new(origin: Pt3, direction: Vec3) -> Self {
        Ray { origin, direction }
    }

    pub fn origin(self) -> Pt3 {
        self.origin
    }

    pub fn direction(self) -> Vec3 {
        self.direction
    }

    pub fn point_at_parameter(self, t: f32) -> Pt3 {
        self.origin + t * self.direction
    }
}
