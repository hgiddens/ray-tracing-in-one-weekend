#include "material.hpp"

#include <cassert>
#include <cmath>

#include "object.hpp"
#include "ray.hpp"

namespace {
    vec3 reflect(vec3 const& v, vec3 const& n) {
        return v - 2 * v.dot(n) * n;
    }

    std::optional<vec3> refract(vec3 const& v, vec3 const& n, double const ni_over_nt) {
        vec3 const uv = v.unit_vector();
        double const
            dt = uv.dot(n),
            discriminant = 1 - ni_over_nt * ni_over_nt * (1 - dt * dt);
        if (discriminant > 0) {
            return {ni_over_nt * (uv - n * dt) - n * std::sqrt(discriminant)};
        } else {
            return {};
        }
    }

    double schlick(double const cosine, double const refractive_index) {
        double const r0 = (1 - refractive_index) / (1 + refractive_index);
        double const r1 = r0 * r0;
        return r1 + (1 - r1) * std::pow(1 - cosine, 5);
    }
}

material::material(std::mt19937& mt) : mt(mt) {}

vec3 material::random_in_unit_sphere() const {
    while (true) {
        vec3 const candidate{dist(mt), dist(mt), dist(mt)};
        if (candidate.squared_length() < 1) return candidate;
    }
}

std::uniform_real_distribution<double> material::dist{-1, 1};

lambertian::lambertian(std::mt19937& mt, vec3 const& albedo)
    : material(mt), albedo(albedo) {}

std::optional<scatter_record> lambertian::scatter(ray const& r [[gnu::unused]], hit_record const& hit) const {
    vec3 const target = hit.p + hit.normal + random_in_unit_sphere();
    return {{albedo, {hit.p, target - hit.p}}};
}

metal::metal(std::mt19937& mt, vec3 const& albedo, double const fuzziness)
    : material(mt), albedo(albedo), fuzziness(fuzziness) {
    assert(fuzziness <= 1);
}

std::optional<scatter_record> metal::scatter(ray const& r, hit_record const& hit) const {
    vec3 const reflected = reflect(r.direction().unit_vector(), hit.normal);
    ray const scattered{hit.p, reflected + fuzziness * random_in_unit_sphere()};
    if (scattered.direction().dot(hit.normal) > 0) {
        return {{albedo, scattered}};
    } else {
        return {};
    }
}

std::uniform_real_distribution<double> dielectric::reflect_dist{0, 1};

dielectric::dielectric(std::mt19937& mt, double const refractive_index)
    : material(mt), refractive_index(refractive_index) {
    // TODO: does this make sense?
    assert(refractive_index >= 0);
}

std::optional<scatter_record> dielectric::scatter(ray const& r, hit_record const& hit) const {
    bool const simple = r.direction().dot(hit.normal) > 0;
    vec3 const
        outward_normal = simple ? -hit.normal : hit.normal,
        reflected = reflect(r.direction(), hit.normal),
        attenuation{1, 1, 1};
    double const
        ni_over_nt = simple ? refractive_index : 1 / refractive_index,
        cosine = simple ?
            refractive_index * r.direction().dot(hit.normal) / r.direction().length() :
            -r.direction().dot(hit.normal) / r.direction().length();
    
    auto const refracted = refract(r.direction(), outward_normal, ni_over_nt);

    double const reflect_prob = refracted ? schlick(cosine, refractive_index) : 1.0;
    if (reflect_dist(mt) < reflect_prob) {
        return {{attenuation, {hit.p, reflected}}};
    } else {
        assert(refracted);
        return {{attenuation, {hit.p, *refracted}}};
    }
}
