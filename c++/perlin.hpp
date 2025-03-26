#pragma once

#include <cstdint>
#include <vector>

#include "vec3.hpp"

class perlin {
    // TODO: These should be std::array
    static std::vector<double> const ranfloat;
    static std::vector<uint8_t> const perm_x, perm_y, perm_z;
public:
    double noise(vec3 const& p) const;
};
