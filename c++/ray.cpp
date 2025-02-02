#include "ray.hpp"

ray::ray(vec3 const origin, vec3 const direction, double const time)
    : a(origin), b(direction), t0(time) {}

vec3 ray::point_at_parameter(double const t) const {
    return a + t*b;
}
