#pragma once

class vec3 final {
    double elements[3];
public:
    vec3(double x, double y, double z);
    double x() const { return elements[0]; }
    double y() const { return elements[1]; }
    double z() const { return elements[2]; }

    vec3 operator-() const;

    vec3 cross(vec3 const& that) const;
    double dot(vec3 const& that) const;
    double length() const;
    double squared_length() const;
    vec3 unit_vector() const;
};

vec3 operator+(vec3 const& a, vec3 const& b);

vec3 operator-(vec3 const& a, vec3 const& b);

vec3 operator*(vec3 const& v, double d);
vec3 operator*(double d, vec3 const& v);

vec3 operator/(vec3 const& v, double d);
