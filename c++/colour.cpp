#include "colour.hpp"

#include <cassert>
#include <cmath>

colour::colour(double white) : colour(white, white, white) {}

colour::colour(double const red, double const green, double const blue) {
    // TODO: types
    assert(red >= 0 and red <= 1);
    assert(green >= 0 and green <= 1);
    assert(blue >= 0 and blue <= 1);

    elements[0] = red;
    elements[1] = green;
    elements[2] = blue;
}

colour colour::gamma2() const {
    return { std::sqrt(red()), std::sqrt(green()), std::sqrt(blue()) };
}

colour operator+(colour a, colour const b) {
    a.elements[0] += b.red();
    a.elements[1] += b.green();
    a.elements[2] += b.blue();
    return a;
}

colour operator*(colour c, double const d) {
    assert(d >= 0);
    assert(d <= 1);
    c.elements[0] *= d;
    c.elements[1] *= d;
    c.elements[2] *= d;
    return c;
}

colour operator*(double const d, colour c) {
    assert(d >= 0);
    assert(d <= 1);
    c.elements[0] *= d;
    c.elements[1] *= d;
    c.elements[2] *= d;
    return c;
}
