use crate::vec3::Vec3;

#[derive(Clone, Copy, Debug)]
pub struct Pt3 {
    // Best practise in Rust seems to be avoid getters in the absence of
    // invariants, so this is fine, I think?
    pub x: f32,
    pub y: f32,
    pub z: f32,
}

impl std::ops::Add<Vec3> for Pt3 {
    type Output = Self;

    fn add(self, v: Vec3) -> Self::Output {
        Pt3 {
            x: self.x + v.x,
            y: self.y + v.y,
            z: self.z + v.z,
        }
    }
}

impl std::ops::Sub for Pt3 {
    type Output = Vec3;

    fn sub(self, other: Self) -> Self::Output {
        Vec3::new(self.x - other.x, self.y - other.y, self.z - other.z)
    }
}

impl std::ops::Sub<Vec3> for Pt3 {
    type Output = Self;

    fn sub(self, v: Vec3) -> Self::Output {
        Pt3 {
            x: self.x - v.x,
            y: self.y - v.y,
            z: self.z - v.z,
        }
    }
}
