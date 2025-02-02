#pragma once

#include "ray.hpp"
#include "vec3.hpp"

inline double ffmin(double a, double b) { return a < b ? a : b; }
inline double ffmax(double a, double b) { return a > b ? a : b; }

class aabb final {
    vec3 _min, _max;
public:
    static aabb surrounding_box(aabb box0, aabb box1);

    aabb(vec3 a, vec3 b);

    vec3 min() const { return _min; }
    vec3 max() const { return _max; }

    bool hit(ray const& r, double tmin, double tmax) const;
};
