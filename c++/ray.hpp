#pragma once

#include "vec3.hpp"

class ray final {
    vec3 a, b;
public:
    ray(vec3 origin, vec3 direction);

    vec3 origin() const { return a; }
    vec3 direction() const { return b; }
    vec3 point_at_parameter(double t) const;
};
