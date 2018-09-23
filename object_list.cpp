#include "object_list.hpp"

#include <cassert>

object_list::object_list(std::vector<std::unique_ptr<object>>&& objects)
    : objects(std::move(objects)) {}

std::optional<hit_record> object_list::hit(ray const& r, double const t_min, double const t_max) const {
    assert(t_min <= t_max);

    // TODO: this is dumb
    hit_record const* closest_hit = nullptr;
    double closest_so_far = t_max;
    for (auto const& object : objects) {
        auto const hit = object->hit(r, t_min, closest_so_far);
        if (hit) {
            delete closest_hit;
            closest_hit = new hit_record(*hit);
            closest_so_far = hit->t;
        }
    }

    if (closest_hit == nullptr) {
        return {};
    } else {
        std::optional<hit_record> result{*closest_hit};
        delete closest_hit;
        return result;
    }
}
