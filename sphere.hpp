#pragma once

#include "object.hpp"

class sphere final : public object {
public:
    vec3 const centre;
    double const radius;
public:
    sphere(vec3 const& centre, double radius);
    std::optional<hit_record> hit(ray const& r, double t_min, double t_max) const override;
};
