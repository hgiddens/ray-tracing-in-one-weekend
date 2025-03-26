#pragma once

#include <memory>
#include <utility>

#include "colour.hpp"
#include "vec3.hpp"

class texture {
public:
    virtual ~texture() = default;
    virtual colour value(double u, double v, vec3 const& p) const = 0;
};

class constant_texture : public texture {
    colour c;
public:
    explicit constant_texture(colour c) : c(c) {}
    colour value(double u [[gnu::unused]], double v [[gnu::unused]], vec3 const& p [[gnu::unused]]) const override { return c; }
};

class chequer_texture : public texture {
    std::unique_ptr<texture const> even;
    std::unique_ptr<texture const> odd;
public:
    chequer_texture(std::unique_ptr<texture const> even, std::unique_ptr<texture const> odd);
    colour value(double u, double v, vec3 const& p) const override;
};

class perlin_texture : public texture {
public:
    colour value(double u, double v, vec3 const& p) const override;
};
