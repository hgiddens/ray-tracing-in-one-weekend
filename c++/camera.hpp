#pragma once

#include <random>

#include "ray.hpp"

class camera final {
    static std::uniform_real_distribution<double> dist;

    vec3 const origin;
    vec3 const w;
    vec3 const u;
    vec3 const v;
    vec3 const lower_left_corner;
    vec3 const horizontal;
    vec3 const vertical;
    double lens_radius;

    std::mt19937& mt;

    vec3 random_in_unit_disk() const;

public:
    // vfov = vertical field of view in degrees
    camera(std::mt19937& mt, vec3 from, vec3 at, vec3 up, double vfov, double aspect, double aperture, double focus_dist);
    ray get_ray(double s, double t) const;
};
