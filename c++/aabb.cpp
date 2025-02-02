#include "aabb.hpp"

#include <cmath>
#include <utility>

aabb::aabb(vec3 const a, vec3 const b) : _min(a), _max(b) {}

bool aabb::hit(ray const& r, double tmin, double tmax) const {
    // Inlined for the same reasons as in Common Lisp, except that here I
    // haven't bothered to benchmark it.
    {
        double const invD = 1.0 / r.direction().x();
        double
            t0 = (min().x() - r.origin().x()) * invD,
            t1 = (max().x() - r.origin().x()) * invD;
        if (invD < 0) {
            std::swap(t0, t1);
        }
        tmin = ffmax(t0, tmin);
        tmax = ffmin(t1, tmax);
        if (tmax <= tmin) {
            return false;
        }
    }

    {
        double const invD = 1.0 / r.direction().y();
        double
            t0 = (min().y() - r.origin().y()) * invD,
            t1 = (max().y() - r.origin().y()) * invD;
        if (invD < 0) {
            std::swap(t0, t1);
        }
        tmin = ffmax(t0, tmin);
        tmax = ffmin(t1, tmax);
        if (tmax <= tmin) {
            return false;
        }
    }

    {
        double const invD = 1.0 / r.direction().z();
        double
            t0 = (min().z() - r.origin().z()) * invD,
            t1 = (max().z() - r.origin().z()) * invD;
        if (invD < 0) {
            std::swap(t0, t1);
        }
        tmin = ffmax(t0, tmin);
        tmax = ffmin(t1, tmax);
        if (tmax <= tmin) {
            return false;
        }
    }

    return true;
}

aabb aabb::surrounding_box(aabb box0, aabb box1) {
    vec3 const
        small{std::fmin(box0.min().x(), box1.min().x()), std::fmin(box0.min().y(), box1.min().y()), std::fmin(box0.min().z(), box1.min().z())},
        big{std::fmax(box0.max().x(), box1.max().x()), std::fmax(box0.max().y(), box1.max().y()), std::fmax(box0.max().z(), box1.max().z())};
    return aabb(small, big);
}
