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

vec3 vec3::cross(vec3 const& that) const {
    return {
        y() * that.z() - z() * that.y(),
        -(x() * that.z() - z() * that.x()),
        x() * that.y() - y() * that.x()
    };
}

double vec3::dot(vec3 const& that) const {
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

vec3 operator+(vec3 const& a, vec3 const& b) {
    return { a.x() + b.x(), a.y() + b.y(), a.z() + b.z() };
}

vec3 operator-(vec3 const& a, vec3 const& b) {
    return { a.x() - b.x(), a.y() - b.y(), a.z() - b.z() };
}

vec3 operator*(vec3 const& v, double const d) {
    return { v.x() * d, v.y() * d, v.z() * d };
}

vec3 operator*(double const d, vec3 const& v) {
    return v * d;
}

vec3 operator/(vec3 const& v, double d) {
    assert(d != 0);
    return { v.x() / d, v.y() / d, v.z() / d };
}
