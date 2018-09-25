#include "material.hpp"

#include <cassert>

#include "object.hpp"
#include "ray.hpp"

namespace {
    vec3 reflect(vec3 const& v, vec3 const& n) {
        return v - 2 * v.dot(n) * n;
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
