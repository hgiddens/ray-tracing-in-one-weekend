#pragma once

#include <memory>

#include "material.hpp"
#include "object.hpp"

class moving_sphere final : public object {
    vec3 centre0, centre1;
    double time0, time1;
    double radius;
    std::unique_ptr<material const> mat;

    vec3 centre(double time) const;
public:
    moving_sphere(vec3 centre0, vec3 centre1, double time0, double time1, double radius, std::unique_ptr<material const> mat);
    std::optional<hit_record> hit(ray const& r, double t_min, double t_max) const override;
};
