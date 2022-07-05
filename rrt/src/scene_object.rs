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
