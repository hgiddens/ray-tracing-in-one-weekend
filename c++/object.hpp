#pragma once

#include <optional>

#include "aabb.hpp"
#include "ray.hpp"

class material;

struct hit_record {
    double t;
    vec3 p;
    vec3 normal;
    material const* material;
};

class object {
public:
    virtual ~object() = default;
    virtual std::optional<hit_record> hit(ray const& r, double t_min, double t_max) const = 0;
    virtual std::optional<aabb> bounding_box(double t0, double t1) const = 0;
};
