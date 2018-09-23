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

colour operator+(colour const& a, colour const& b) {
    return { a.red() + b.red(), a.green() + b.green(), a.blue() + b.blue() };
}

colour operator*(colour const& c, double d) {
    assert(d >= 0);
    assert(d <= 1);
    return { c.red() * d, c.green() * d, c.blue() * d };
}

colour operator*(double d, colour const& c) {
    return c * d;
}
