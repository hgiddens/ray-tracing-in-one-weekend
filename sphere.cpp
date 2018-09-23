#include "sphere.hpp"

#include <cassert>
#include <cmath>

sphere::sphere(vec3 const& centre, double radius)
    : centre(centre), radius(radius) {}

std::optional<hit_record> sphere::hit(ray const& r, double const t_min, double const t_max) const {
    assert(t_min <= t_max);
    
    vec3 const oc = r.origin() - centre;
    double const
        a = r.direction().dot(r.direction()),
        b = oc.dot(r.direction()),
        c = oc.dot(oc) - radius * radius,
        discriminant = b * b - a * c,
        discriminant_root = std::sqrt(discriminant);

    if (discriminant > 0) {
        double const first_hit = (-b - discriminant_root) / a;
        if (first_hit < t_max && first_hit > t_min) {
            auto const hit = r.point_at_parameter(first_hit);
            return {{
                first_hit,
                hit,
                (hit - centre) / radius
            }};
        }

        double const second_hit = (-b + discriminant_root) / a;
        if (second_hit < t_max && second_hit > t_min) {
            auto const hit = r.point_at_parameter(second_hit);
            return {{
                second_hit,
                hit,
                (hit - centre) / radius
            }};
        }
    }

    return {};
}
