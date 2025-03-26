#include "texture.hpp"

#include <cmath>

#include "perlin.hpp"

chequer_texture::chequer_texture(std::unique_ptr<texture const> even, std::unique_ptr<texture const> odd)
    : even(std::move(even)), odd(std::move(odd)) {}

colour chequer_texture::value(double const u, double const v, vec3 const& p) const {
    double const sines = std::sin(10 * p.x()) * std::sin(10 * p.y()) * std::sin(10 * p.z());
    return (sines < 0 ? odd : even)->value(u, v, p);
}

colour perlin_texture::value(double const u, double const v, vec3 const& p) const {
    double const grey = perlin{}.noise(p);
    return {grey, grey, grey};
}
