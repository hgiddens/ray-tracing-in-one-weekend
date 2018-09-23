#include <cmath>
#include <iostream>
#include <limits>
#include <optional>
#include <vector>

#include "colour.hpp"
#include "object_list.hpp"
#include "ray.hpp"
#include "sphere.hpp"

namespace {
    // convert [0, 1] colour plane to [0, 256)
    int convert(double const f) {
        return int(255.99*f);
    }

    colour ray_colour(ray const& r, object const& world) {
        auto const result = world.hit(r, 0, std::numeric_limits<double>::max());
        if (result) {
            auto const& normal = result->normal;
            auto const result = 0.5 * vec3{normal.x() + 1, normal.y() + 1, normal.z() + 1};
            return { result.x(), result.y(), result.z() };
        } else {
            auto const unit_direction = r.direction().unit_vector();
            auto const t = 0.5 * (unit_direction.y() + 1.0);
            colour const white{1.0}, blue{0.5, 0.7, 1.0};
            return (1.0 - t) * white + t * blue;
        }
    }
}

// eye is at origin, center of image
// y axis goes up
// x axis goes right
// into the screen is negative z

int main() {
    int const nx = 1000, ny = 500;

    std::cout << "P3\n" << nx << " " << ny << "\n255\n";

    vec3 const
        lower_left_corner{-2.0, -1.0, -1.0}, // point
        horizontal{4.0, 0.0, 0.0}, // width of the viewport
        vertical{0.0, 2.0, 0.0}, // height of the viewport
        origin{0.0, 0.0, 0.0};

    std::vector<std::unique_ptr<object>> objects;
    objects.emplace_back(new sphere{{0, 0, -1}, 0.5});
    objects.emplace_back(new sphere{{0, -100.5, -1}, 100});
    object_list const world{std::move(objects)};
    
    for (int j = ny - 1; j >= 0; --j) {
        for (int i = 0; i < nx; ++i) {
            double const
                u = double(i) / double(nx),
                v = double(j) / double(ny);
            // This is *not* a unit vector which apparently allows for simpler/faster code?
            vec3 const direction = lower_left_corner + u * horizontal + v * vertical;
            ray const ray{origin, direction};
            colour const colour = ray_colour(ray, world);
            std::cout << convert(colour.red()) << " "
                      << convert(colour.green()) << " "
                      << convert(colour.blue()) << "\n";
        }
    }
}
