#include "camera.hpp"

camera::camera() :
    origin{0, 0, 0},
    lower_left_corner{-2, -1, -1},
    horizontal{4, 0, 0},
    vertical{0, 2, 0} {}

ray camera::get_ray(double const u, double const v) const {
    return {
        origin,
        // This is *not* a unit vector which apparently allows for simpler/faster code?
        lower_left_corner + u * horizontal + v * vertical - origin
    };
}
