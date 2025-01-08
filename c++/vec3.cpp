#include "vec3.hpp"

#include <cassert>
#include <cmath>

vec3::vec3(double x, double y, double z) {
    elements[0] = x;
    elements[1] = y;
    elements[2] = z;
}

vec3 vec3::operator-() const {
    return {-x(), -y(), -z()};
}

vec3 vec3::cross(vec3 const that) const {
    return {
        y() * that.z() - z() * that.y(),
        -(x() * that.z() - z() * that.x()),
        x() * that.y() - y() * that.x()
    };
}

double vec3::dot(vec3 const that) const {
    return x() * that.x() + y() * that.y() + z() * that.z();
}

double vec3::length() const {
    return std::sqrt(squared_length());
}

double vec3::squared_length() const {
    return x() * x() + y() * y() + z() * z();
}

vec3 vec3::unit_vector() const {
    return *this / length();
}

vec3 operator+(vec3 a, vec3 const b) {
    a.elements[0] += b.x();
    a.elements[1] += b.y();
    a.elements[2] += b.z();
    return a;
}

vec3 operator-(vec3 a, vec3 const b) {
    a.elements[0] -= b.x();
    a.elements[1] -= b.y();
    a.elements[2] -= b.z();
    return a;
}

vec3 operator*(vec3 v, double const d) {
    v.elements[0] *= d;
    v.elements[1] *= d;
    v.elements[2] *= d;
    return v;
}

vec3 operator*(double const d, vec3 v) {
    v.elements[0] *= d;
    v.elements[1] *= d;
    v.elements[2] *= d;
    return v;
}

vec3 operator/(vec3 v, double d) {
    assert(d != 0);
    v.elements[0] /= d;
    v.elements[1] /= d;
    v.elements[2] /= d;
    return v;
}
