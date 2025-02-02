#include "object_list.hpp"

#include <cassert>

object_list::object_list(std::unique_ptr<object const> object) {
    objects.push_back(std::move(object));
}

object_list::object_list(std::vector<std::unique_ptr<object const>> objects)
    : objects(std::move(objects)) {}

std::optional<hit_record> object_list::hit(ray const& r, double const t_min, double const t_max) const {
    assert(t_min <= t_max);

    std::optional<hit_record> closest_hit;
    double closest_so_far = t_max;
    for (auto const& object : objects) {
        auto const hit = object->hit(r, t_min, closest_so_far);
        if (hit.has_value()) {
            closest_hit = hit;
            closest_so_far = hit->t;
        }
    }

    return closest_hit;
}

std::optional<aabb> object_list::bounding_box(double t0, double t1) const {
    auto it = objects.begin();
    if (it == objects.end()) {
        return std::nullopt;
    }

    std::optional<aabb> first_result = (**it).bounding_box(t0, t1);
    if (first_result == std::nullopt) {
        return std::nullopt;
    }

    while (++it != objects.end()) {
        std::optional<aabb> next = (**it).bounding_box(t0, t1);
        if (next == std::nullopt) {
            return std::nullopt;
        }

        *first_result = aabb::surrounding_box(*first_result, *next);
    }
    return first_result;
}
