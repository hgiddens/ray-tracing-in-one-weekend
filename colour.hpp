#pragma once

class colour final {
    double elements[3];
public:
    explicit colour(double white);
    colour(double red, double green, double blue);
    double red() const { return elements[0]; }
    double green() const { return elements[1]; }
    double blue() const { return elements[2]; }
};

colour operator+(colour const& a, colour const& b);

colour operator*(colour const& c, double d);
colour operator*(double d, colour const& c);
