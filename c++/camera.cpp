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
}

vec3 camera::random_in_unit_disk() const {
    while (true) {
        vec3 p = vec3{dist(mt), dist(mt), 0};
        if (p.dot(p) < 1) return p;
    }
}

std::uniform_real_distribution<double> camera::dist{-1, 1};

camera::camera(std::mt19937& mt, vec3 const from, vec3 const at, vec3 const up, double const vfov, double const aspect, double const aperture, double const focus_dist, double const t0, double const t1)
    : origin{from},
      w{(from - at).unit_vector()},
      u{up.cross(w).unit_vector()},
      v{w.cross(u)},
      lower_left_corner{from - half_width(vfov, aspect) * focus_dist * u - half_height(vfov) * focus_dist * v - focus_dist * w},
      horizontal{2 * half_width(vfov, aspect) * focus_dist * u},
      vertical{2 * half_height(vfov) * focus_dist * v},
      lens_radius{aperture / 2},
      t0(t0),
      t1(t1),
      mt(mt) {}

ray camera::get_ray(double const s, double const t) const {
    vec3 const rd = lens_radius * random_in_unit_disk();
    vec3 const offset = u * rd.x() + v * rd.y();
    double const time = t0 + dist(mt)*(t1 - t0);
    return {
        origin + offset,
        // This is *not* a unit vector which apparently allows for simpler/faster code?
        lower_left_corner + s * horizontal + t * vertical - origin - offset,
        time
    };
}
