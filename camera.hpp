#pragma once

#include "ray.hpp"

class camera final {
    vec3 const origin;
    vec3 const lower_left_corner;
    vec3 const horizontal;
    vec3 const vertical;
public:
    camera();
    ray get_ray(double u, double v) const;
};
