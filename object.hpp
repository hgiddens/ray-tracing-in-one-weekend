#pragma once

#include <optional>

#include "ray.hpp"

struct hit_record {
    double const t;
    vec3 const p;
    vec3 const normal;
};

class object {
public:
    virtual ~object() {}
    virtual std::optional<hit_record> hit(ray const& r, double t_min, double t_max) const = 0;
};
