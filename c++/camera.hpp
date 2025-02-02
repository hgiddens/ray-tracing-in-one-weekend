#pragma once

#include <random>

#include "ray.hpp"

class camera final {
    static std::uniform_real_distribution<double> dist;

    vec3 origin;
    vec3 w;
    vec3 u;
    vec3 v;
    vec3 lower_left_corner;
    vec3 horizontal;
    vec3 vertical;
    double lens_radius;
    double t0, t1;

    std::mt19937& mt;

    vec3 random_in_unit_disk() const;

public:
    // vfov = vertical field of view in degrees
    camera(std::mt19937& mt, vec3 from, vec3 at, vec3 up, double vfov, double aspect, double aperture, double focus_dist, double t0, double t1);
    ray get_ray(double s, double t) const;
};
