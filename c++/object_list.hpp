#pragma once

#include <memory>
#include <vector>

#include "object.hpp"

class object_list final : public object {
    std::vector<std::unique_ptr<object>> objects;
public:
    object_list(std::vector<std::unique_ptr<object>>&& objects);
    std::optional<hit_record> hit(ray const& r, double t_min, double t_max) const override;
};
