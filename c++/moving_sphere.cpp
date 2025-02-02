#include "moving_sphere.hpp"

vec3 moving_sphere::centre(double const time) const {
    return centre0 + ((time - time0) / (time1 - time0)) * (centre1 - centre0);
}

moving_sphere::moving_sphere(
    vec3 const centre0, vec3 const centre1,
    double const time0, double const time1,
    double radius, std::unique_ptr<material const> mat) :
    centre0(centre0), centre1(centre1), time0(time0), time1(time1), radius(radius), mat(std::move(mat)) {}

std::optional<hit_record> moving_sphere::hit(ray const& r, double t_min, double t_max) const {
    vec3 const centre_at_time= centre(r.time());
    vec3 const oc = r.origin() - centre_at_time;

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
                (hit - centre_at_time) / radius,
                mat.get()
            }};
        }

        double const second_hit = (-b + discriminant_root) / a;
        if (second_hit < t_max && second_hit > t_min) {
            auto const hit = r.point_at_parameter(second_hit);
            return {{
                second_hit,
                hit,
                (hit - centre_at_time) / radius,
                mat.get()
            }};
        }
    }

    return std::nullopt;
}
