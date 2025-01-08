#pragma once

class colour final {
    double elements[3];
public:
    explicit colour(double white);
    colour(double red, double green, double blue);
    double red() const { return elements[0]; }
    double green() const { return elements[1]; }
    double blue() const { return elements[2]; }

    colour gamma2() const;

    friend colour operator+(colour a, colour b);
    friend colour operator*(colour c, double d);
    friend colour operator*(double d, colour c);
};

