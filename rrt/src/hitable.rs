use crate::pt3::Pt3;
use crate::ray::Ray;
use crate::vec3::Vec3;

pub struct HitRecord {
    pub t: f32,
    pub p: Pt3,
    pub normal: Vec3,
}

pub trait Hitable {
    fn hit(&self, ray: &Ray, t_min: f32, t_max: f32) -> Option<HitRecord>;
}

// TODO: I fear this will bring me nothing but woe lol.
// If I have e.g. spheres and cubes, would like to shove them together in a list.
// Maybe type erasure? Otherwise can presumably achieve with Dyn or Box?
// TODO: Implement for any iterable.
// There's no trait providing iter and IntoIterable got me lost in lifetime fun.
// Find out what the actual solution to this is?
impl<A> Hitable for Vec<A>
where
    A: Hitable,
{
    fn hit(&self, ray: &Ray, t_min: f32, t_max: f32) -> Option<HitRecord> {
        let mut earliest_hit: Option<HitRecord> = None;

        for h in self {
            if let Some(h) = h.hit(
                ray,
                t_min,
                earliest_hit.as_ref().map(|h| h.t).unwrap_or(t_max),
            ) {
                earliest_hit = Some(h)
            }
        }

        earliest_hit
    }
}
