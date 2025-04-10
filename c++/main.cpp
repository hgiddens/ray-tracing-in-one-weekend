#include <chrono>
#include <cmath>
#include <iostream>
#include <limits>
#include <memory>
#include <optional>
#include <random>
#include <vector>

#include "bvh_node.hpp"
#include "camera.hpp"
#include "colour.hpp"
#include "material.hpp"
#include "moving_sphere.hpp"
#include "object_list.hpp"
#include "ray.hpp"
#include "sphere.hpp"
#include "supersampler.hpp"
#include "texture.hpp"

namespace {
    // deal with shadow acne
    const double time_epsilon = 0.001;
    
    // convert [0, 1] colour plane to [0, 256)
    int convert(double const f) {
        return int(255.99*f);
    }

    colour ray_colour(ray const& r, object const& world, int const depth) {
        auto const result = world.hit(r, time_epsilon, std::numeric_limits<double>::max());
        if (result.has_value()) {
            std::optional<scatter_record> scatter = result->material->scatter(r, *result);
            if (depth < 50 && scatter.has_value()) {
                auto const next = ray_colour(scatter->scattered, world, depth + 1);
                return {
                    next.red() * scatter->attenuation.x(),
                    next.green() * scatter->attenuation.y(),
                    next.blue() * scatter->attenuation.z()
                };
            } else {
                return colour{0};
            }
        } else {
            auto const unit_direction = r.direction().unit_vector();
            auto const t = 0.5 * (unit_direction.y() + 1.0);
            colour const white{1.0}, blue{0.5, 0.7, 1.0};
            return (1.0 - t) * white + t * blue;
        }
    }

    object_list two_perlin_spheres(std::mt19937& mt) {
        std::vector<std::unique_ptr<object const>> objects;

        objects.push_back(std::make_unique<sphere const>(vec3{0, -1000, 0}, 1000, std::make_unique<lambertian const>(mt, std::make_unique<perlin_texture const>())));
        objects.push_back(std::make_unique<sphere const>(vec3{0, 2, 0}, 2, std::make_unique<lambertian const>(mt, std::make_unique<perlin_texture const>())));

        return object_list(std::move(objects));
    }

    bvh_node build_world(std::mt19937& mt) {
        std::vector<std::unique_ptr<object const>> objects;
        objects.reserve(500);  // 488-ish
        auto chequer = std::make_unique<chequer_texture const>(
            std::make_unique<constant_texture const>(colour{0.2, 0.3, 0.1}),
            std::make_unique<constant_texture const>(colour{0.9, 0.9, 0.9}));
        objects.push_back(std::make_unique<sphere const>(vec3{0, -1000, -1}, 1000, std::make_unique<lambertian const>(mt, std::move(chequer))));
        objects.push_back(std::make_unique<sphere const>(vec3{0, 1, 0}, 1, std::make_unique<dielectric>(mt, 1.5)));
        objects.push_back(std::make_unique<sphere const>(vec3{-4, 1, 0}, 1, std::make_unique<lambertian>(mt, std::make_unique<constant_texture const>(colour{0.4, 0.2, 0.1}))));
        objects.push_back(std::make_unique<sphere const>(vec3{4, 1, 0}, 1, std::make_unique<metal>(mt, vec3{0.7, 0.6, 0.5}, 0)));

        std::uniform_real_distribution<double> dist{0, 1};
        for (int a = -11; a < 11; ++a) {
            for (int b = -11; b < 11; ++b) {
                vec3 const centre{a + 0.9*dist(mt), 0.2, b + 0.9*dist(mt)};
                if ((centre - vec3{4, 0.2, 0}).length() <= 0.9) {
                    continue;
                }

                double const choose_mat = dist(mt);
                if (choose_mat < 0.8) {
                    vec3 const offset{0, 0.5 * dist(mt), 0};
                    colour const c{dist(mt) * dist(mt), dist(mt) * dist(mt), dist(mt) * dist(mt)};
                    auto t = std::make_unique<constant_texture const>(c);
                    objects.push_back(std::make_unique<moving_sphere const>(centre, centre + offset, 0, 1, 0.2,
                                                                            std::make_unique<lambertian const>(mt, std::move(t))));
                } else if (choose_mat < 0.95) {
                    objects.push_back(std::make_unique<sphere const>(centre, 0.2,
                                                                     std::make_unique<const metal>(mt, vec3{0.5*(1 + dist(mt)), 0.5*(1 + dist(mt)), 0.5*(1 + dist(mt))}, 0.5*dist(mt))));
                } else {
                    objects.push_back(std::make_unique<sphere const>(centre, 0.2,
                                                                     std::make_unique<const dielectric>(mt, 1.5)));
                }
            }
        }

        // TODO: I think bvh_node obsoletes object_list?
        return bvh_node(mt, objects, 0, 1);
    }
}

// eye is at origin, center of image
// y axis goes up
// x axis goes right
// into the screen is negative z

int main() {
    int const nx = 400, ny = 200, ns = 10;

    std::random_device rd;
    std::mt19937 mt(rd());
    std::uniform_real_distribution<double> dist{0, 1};
    object const& world = two_perlin_spheres(mt);
    vec3 const
        from{13, 2, 3},
        at{0, 0, 0};
    double const
        dist_to_focus = 10,
        aperture = 0;
    camera camera{mt, from, at, vec3{0, 1, 0}, 20, double(nx) / double(ny), aperture, dist_to_focus, 0, 1};

    std::vector<colour> buffer;
    buffer.reserve(nx * ny);

    const auto render_start = std::chrono::steady_clock::now();
    for (int j = ny - 1; j >= 0; --j) {
        for (int i = 0; i < nx; ++i) {
            supersampler supersampler;
            for (int s = 0; s < ns; ++s) {
                double const
                    ii = i + dist(mt),
                    jj = j + dist(mt),
                    u = ii / double(nx),
                    v = jj / double(ny);
                ray const ray = camera.get_ray(u, v);
                supersampler.add_sample(ray_colour(ray, world, 0));
            }

            buffer.push_back(supersampler.value().gamma2());
        }
    }
    const auto render_end = std::chrono::steady_clock::now();
    std::cerr << "Render duration: " << std::chrono::duration<double>{render_end - render_start} << "\n";

    std::cout << "P3\n" << nx << " " << ny << "\n255\n";
    for (colour const& colour : buffer) {
        std::cout << convert(colour.red()) << " "
                  << convert(colour.green()) << " "
                  << convert(colour.blue()) << "\n";
    }
}
