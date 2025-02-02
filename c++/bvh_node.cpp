#include "bvh_node.hpp"

#include <algorithm>
#include <cassert>

std::uniform_int_distribution<int> bvh_node::dist{0, 2};

bvh_node::bvh_node(std::mt19937& mt, std::span<std::unique_ptr<object const>> objects, double const t0, double const t1) {
    int const axis = dist(mt);
    std::ranges::sort(objects, [axis, t0, t1](std::unique_ptr<object const> const& l, std::unique_ptr<object const> const& r) -> bool {
        // Diff from book: fixed to use real time values.
        std::optional<aabb>
            box_left = l != nullptr ? l->bounding_box(t0, t1) : std::nullopt,
            box_right = r != nullptr ? r->bounding_box(t0, t1) : std::nullopt;
        if (box_left == std::nullopt || box_right == std::nullopt) {
            throw "no bounding box in bvh_node constructor";
        }
        switch (axis) {
        case 0:
            return box_left->min().x() < box_right->min().x();
        case 1:
            return box_left->min().y() < box_right->min().y();
        case 2:
            return box_left->min().z() < box_right->min().z();
        default:
            throw "something has gone terribly wrong";
        }
    });

    assert(objects.size() > 0);
    if (objects.size() == 1) {
        left = std::move(objects[0]);
    } else if (objects.size() == 2) {
        left = std::move(objects[0]);
        right = std::move(objects[1]);
    } else {
        left = std::make_unique<bvh_node const>(mt, objects.subspan(0, objects.size() / 2), t0, t1);
        right = std::make_unique<bvh_node const>(mt, objects.subspan(objects.size() / 2), t0, t1);
    }

    if (right == nullptr) {
        std::optional<aabb> box_left = left->bounding_box(t0, t1);
        if (box_left == std::nullopt) {
            throw "no bounding box in bvh_node constructor";
        }
        box = *box_left;
    } else {
        std::optional<aabb> const
            box_left = left->bounding_box(t0, t1),
            box_right = right->bounding_box(t0, t1);
        if (box_left == std::nullopt || box_right == std::nullopt) {
            throw "no bounding box in bvh_node constructor";
        }
        box = aabb::surrounding_box(*box_left, *box_right);
    }
}

std::optional<hit_record> bvh_node::hit(ray const& r, double const t_min, double const t_max) const {
    if (box == std::nullopt || !box->hit(r, t_min, t_max)) {
        return std::nullopt;
    }

    std::optional<hit_record>
        hit_left = left != nullptr ? left->hit(r, t_min, t_max) : std::nullopt,
        hit_right = right != nullptr ? right->hit(r, t_min, t_max) : std::nullopt;
    if (hit_left != std::nullopt && hit_right != std::nullopt) {
        return hit_left->t < hit_right->t ? hit_left : hit_right;
    } else if (hit_left != std::nullopt) {
        return hit_left;
    } else if (hit_right != std::nullopt) {
        return hit_right;
    }

    return std::nullopt;
}

std::optional<aabb> bvh_node::bounding_box(double const t0 [[gnu::unused]], double const t1 [[gnu::unused]]) const {
    return box;
}
