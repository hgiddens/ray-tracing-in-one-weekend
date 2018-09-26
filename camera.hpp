#pragma once

#include "ray.hpp"

class camera final {
    vec3 const origin;
    vec3 const lower_left_corner;
    vec3 const horizontal;
    vec3 const vertical;

public:
    // vfov = vertical field of view in degrees
    camera(vec3 from, vec3 at, vec3 up, double vfov, double aspect);
    ray get_ray(double u, double v) const;
};
