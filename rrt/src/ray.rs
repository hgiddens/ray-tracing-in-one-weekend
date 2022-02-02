use crate::pt3::Pt3;
use crate::vec3::Vec3;

#[derive(Clone, Copy, Debug)]
pub struct Ray {
    origin: Pt3,
    direction: Vec3,
}

impl Ray {
    pub fn from_origin_to(target: Pt3) -> Self {
        let origin = Pt3 {
            x: 0.0,
            y: 0.0,
            z: 0.0,
        };
        let direction = Vec3::new(target.x, target.y, target.z);
        Ray { origin, direction }
    }

    pub fn origin(self) -> Pt3 {
        self.origin
    }

    pub fn direction(self) -> Vec3 {
        self.direction
    }
}
