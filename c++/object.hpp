#pragma once

#include <optional>

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
    virtual ~object() {}
    virtual std::optional<hit_record> hit(ray const& r, double t_min, double t_max) const = 0;
};
