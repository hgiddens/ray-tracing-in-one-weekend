use crate::material::Material;
use crate::pt3::Pt3;
use crate::ray::Ray;
use crate::vec3::Vec3;

pub struct HitRecord<'a> {
    pub t: f32,
    pub p: Pt3,
    pub normal: Vec3,
    pub material: &'a dyn Material,
}

pub trait SceneObject<'a> {
    fn hit(self, ray: &Ray, t_min: f32, t_max: f32) -> Option<HitRecord<'a>>;
}

// This will gives me "any iterable of scene objects is itself a scene object"
// but it's also absolutely fucking insane. This is an apocalypse. This cannot
// be allowed to exist. Unfortunately, however, I'm not sure what a better
// solution looks like. Consider a world in which we didn't have just spheres;
// this then recurses into madness with Box<dyn SceneObject<'a>> or possibly
// something even worse.
// TODO: rethink this, career, life in that order.
impl<'a, Cont, A> SceneObject<'a> for &'a Cont
where
    &'a Cont: IntoIterator<Item = A>,
    A: SceneObject<'a>,
{
    fn hit(self, ray: &Ray, t_min: f32, t_max: f32) -> Option<HitRecord<'a>> {
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
