#pragma once

class vec3 final {
    double elements[3];
public:
    vec3(double x, double y, double z);
    double x() const { return elements[0]; }
    double y() const { return elements[1]; }
    double z() const { return elements[2]; }

    vec3 operator-() const;

    vec3 cross(vec3 that) const;
    double dot(vec3 that) const;
    double length() const;
    double squared_length() const;
    vec3 unit_vector() const;

    friend vec3 operator+(vec3 a, vec3 b);
    friend vec3 operator-(vec3 a, vec3 b);
    friend vec3 operator*(vec3 v, double d);
    friend vec3 operator*(double d, vec3 v);
    friend vec3 operator/(vec3 v, double d);
};

