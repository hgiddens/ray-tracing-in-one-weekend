#include <cmath>
#include <iostream>
#include <limits>
#include <optional>
#include <random>
#include <vector>

#include "camera.hpp"
#include "colour.hpp"
#include "object_list.hpp"
#include "ray.hpp"
#include "sphere.hpp"
#include "supersampler.hpp"

namespace {
    // deal with shadow acne
    const double time_epsilon = 0.001;
    
    // convert [0, 1] colour plane to [0, 256)
    int convert(double const f) {
        return int(255.99*f);
    }

    vec3 random_in_unit_sphere(std::mt19937& mt) {
        std::uniform_real_distribution<double> dist{-1, 1};
        while (true) {
            vec3 const candidate{dist(mt), dist(mt), dist(mt)};
            if (candidate.squared_length() < 1) return candidate;
        }
    }

    colour ray_colour(std::mt19937& mt, ray const& r, object const& world) {
        auto const result = world.hit(r, time_epsilon, std::numeric_limits<double>::max());
        if (result) {
            vec3 const target = result->p + result->normal + random_in_unit_sphere(mt);
            return 0.5 * ray_colour(mt, ray{result->p, target - result->p}, world);
        } else {
            auto const unit_direction = r.direction().unit_vector();
            auto const t = 0.5 * (unit_direction.y() + 1.0);
            colour const white{1.0}, blue{0.5, 0.7, 1.0};
            return (1.0 - t) * white + t * blue;
        }
    }

    object_list build_world() {
        std::vector<std::unique_ptr<object>> objects;
        objects.emplace_back(new sphere{{0, 0, -1}, 0.5});
        objects.emplace_back(new sphere{{0, -100.5, -1}, 100});
        return std::move(objects);
    }
}

// eye is at origin, center of image
// y axis goes up
// x axis goes right
// into the screen is negative z

int main() {
    int const nx = 400, ny = 200, ns = 100;

    std::cout << "P3\n" << nx << " " << ny << "\n255\n";

    object const& world = build_world();
    camera camera;
    std::random_device rd;
    std::mt19937 mt(rd());
    std::uniform_real_distribution<double> dist{0, 1};
    
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
                supersampler.add_sample(ray_colour(mt, ray, world));
            }
            
            colour const colour = supersampler.value().gamma2();
            std::cout << convert(colour.red()) << " "
                      << convert(colour.green()) << " "
                      << convert(colour.blue()) << "\n";
        }
    }
}
