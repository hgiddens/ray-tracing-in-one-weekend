#pragma once

#include <memory>

#include "material.hpp"
#include "object.hpp"

class sphere final : public object {
public:
    vec3 centre;
    double radius;
    std::unique_ptr<material const> mat;
public:
    sphere(vec3 centre, double radius, std::unique_ptr<material const> mat);
    std::optional<hit_record> hit(ray const& r, double t_min, double t_max) const override;
};
