#include <iostream>

#include "colour.hpp"
#include "ray.hpp"

class sphere final {
    vec3 const centre;
    double const radius;
public:
    sphere(vec3 const& centre, double radius);
    bool hit(ray const& r) const;
};

sphere::sphere(vec3 const& centre, double radius) : centre(centre), radius(radius) {}

bool sphere::hit(ray const& ray) const {
    vec3 const oc = ray.origin() - centre;
    double const
        a = ray.direction().dot(ray.direction()),
        b = 2.0 * oc.dot(ray.direction()),
        c = oc.dot(oc) - radius * radius,
        discriminant = b * b - 4 * a * c;
    return discriminant > 0;
}

namespace {
    // convert [0, 1] colour plane to [0, 256)
    int convert(double const f) {
        return int(255.99*f);
    }

    colour ray_colour(ray const& r) {
        sphere const world{vec3{0, 0, -1}, 0.5};
        if (world.hit(r)) return { 1, 0, 0 };
        
        auto const unit_direction = r.direction().unit_vector();
        auto const t = 0.5 * (unit_direction.y() + 1.0);
        colour const white{1.0}, blue{0.5, 0.7, 1.0};
        return (1.0 - t) * white + t * blue;
    }
}

// eye is at origin, center of image
// y axis goes up
// x axis goes right
// into the screen is negative z

int main() {
    int const nx = 200, ny = 100;

    std::cout << "P3\n" << nx << " " << ny << "\n255\n";

    vec3 const
        lower_left_corner{-2.0, -1.0, -1.0}, // point
        horizontal{4.0, 0.0, 0.0}, // width of the viewport
        vertical{0.0, 2.0, 0.0}, // height of the viewport
        origin{0.0, 0.0, 0.0};
    
    for (int j = ny - 1; j >= 0; --j) {
        for (int i = 0; i < nx; ++i) {
            double const
                u = double(i) / double(nx),
                v = double(j) / double(ny);
            ray const ray{origin, lower_left_corner + u * horizontal + v * vertical};
            colour const colour = ray_colour(ray);
            std::cout << convert(colour.red()) << " "
                      << convert(colour.green()) << " "
                      << convert(colour.blue()) << "\n";
        }
    }
}
