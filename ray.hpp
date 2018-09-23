#pragma once

#include "vec3.hpp"

class ray {
    vec3 const a, b;
public:
    ray(vec3 const& origin, vec3 const& direction);

    vec3 origin() const { return a; }
    vec3 direction() const { return b; }
    vec3 point_at_parameter(double t) const;
};
