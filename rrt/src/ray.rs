use crate::pt3::Pt3;
use crate::vec3::Vec3;

#[derive(Clone, Copy, Debug)]
pub struct Ray {
    pub origin: Pt3,
    pub direction: Vec3,
}

impl Ray {
    pub fn point_at_parameter(self, t: f32) -> Pt3 {
        self.origin + t * self.direction
    }
}
