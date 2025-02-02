#pragma once

#include <memory>
#include <vector>

#include "object.hpp"

class object_list final : public object {
    std::vector<std::unique_ptr<object const>> objects;
public:
    explicit object_list(std::unique_ptr<object const> object);
    object_list(std::vector<std::unique_ptr<object const>> objects);
    std::optional<hit_record> hit(ray const& r, double t_min, double t_max) const override;
    std::optional<aabb> bounding_box(double t0, double t1) const override;
};
