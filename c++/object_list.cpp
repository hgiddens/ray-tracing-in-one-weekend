#include "object_list.hpp"

#include <cassert>

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
