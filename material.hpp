#pragma once

#include <optional>
#include <random>

#include "ray.hpp"
#include "vec3.hpp"

struct hit_record;

struct scatter_record {
    vec3 const attenuation;
    ray const scattered;
};

class material {
    std::mt19937& mt;
    static std::uniform_real_distribution<double> dist;

protected:
    material(std::mt19937& mt);
    vec3 random_in_unit_sphere() const;

public:
    virtual ~material() {}
    virtual std::optional<scatter_record> scatter(ray const& ray, hit_record const& hit) const = 0;
};

class lambertian final : public material {
    vec3 const albedo;

public:
    lambertian(std::mt19937& mt, vec3 const& albedo);
    std::optional<scatter_record> scatter(ray const& ray, hit_record const& hit) const override;
};

class metal final : public material {
    vec3 const albedo;
    double const fuzziness;
public:
    metal(std::mt19937& mt, vec3 const& albedo, double fuzziness);
    std::optional<scatter_record> scatter(ray const& ray, hit_record const& hit) const override;
};
