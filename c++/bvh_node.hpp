#pragma once

#include <memory>
#include <optional>
#include <random>
#include <span>

#include "aabb.hpp"
#include "object.hpp"

class bvh_node final : public object {
    static std::uniform_int_distribution<int> dist;

    std::shared_ptr<object const> left, right;
    std::optional<aabb> box;
public:
    bvh_node(std::mt19937& mt, std::span<std::unique_ptr<object const>> objects, double t0, double t1);

    std::optional<hit_record> hit(ray const& r, double t_min, double t_max) const override;
    std::optional<aabb> bounding_box(double t0, double t1) const override;
};
