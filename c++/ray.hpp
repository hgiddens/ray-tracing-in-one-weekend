#pragma once

#include "vec3.hpp"

class ray final {
    vec3 a, b;
    double t0;
public:
    ray(vec3 origin, vec3 direction, double time);

    vec3 origin() const { return a; }
    vec3 direction() const { return b; }
    double time() const { return t0; }
    vec3 point_at_parameter(double t) const;
};
