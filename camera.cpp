#include "camera.hpp"

#include <cmath>

namespace {
    double half_height(double const vfov) {
        double const theta = vfov * M_PI / 180;
        return std::tan(theta / 2);
    }
    
    double half_width(double const vfov, double const aspect) {
        return aspect * half_height(vfov);
    }

    vec3 w(vec3 const from, vec3 const at) {
        return (from - at).unit_vector();
    }

    vec3 u(vec3 const from, vec3 const at, vec3 const up) {
        return up.cross(w(from, at)).unit_vector();
    }

    vec3 v(vec3 const from, vec3 const at, vec3 const up) {
        return w(from, at).cross(u(from, at, up));
    }
}

camera::camera(vec3 const from, vec3 const at, vec3 const up, double const vfov, double const aspect)
    : origin{from},
      lower_left_corner{from - half_width(vfov, aspect) * u(from, at, up) - half_height(vfov) * v(from, at, up) - w(from, at)},
      horizontal{2 * half_width(vfov, aspect) * u(from, at, up)},
      vertical{2 * half_height(vfov) * v(from, at, up)} {}

ray camera::get_ray(double const u, double const v) const {
    return {
        origin,
        // This is *not* a unit vector which apparently allows for simpler/faster code?
        lower_left_corner + u * horizontal + v * vertical - origin
    };
}
