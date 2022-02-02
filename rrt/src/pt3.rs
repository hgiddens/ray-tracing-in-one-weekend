use crate::vec3::Vec3;

#[derive(Clone, Copy, Debug)]
pub struct Pt3 {
    // TODO: Is this dumb?
    pub x: f32,
    pub y: f32,
    pub z: f32,
}

impl Pt3 {
    pub fn origin() -> Self {
        Pt3 {
            x: 0.0,
            y: 0.0,
            z: 0.0,
        }
    }
}

impl std::ops::Sub for Pt3 {
    type Output = Vec3;

    fn sub(self, other: Self) -> Self::Output {
        Vec3::new(self.x - other.x, self.y - other.y, self.z - other.z)
    }
}
