#include "supersampler.hpp"

supersampler::supersampler() : count(0), r(0), g(0), b(0) {}

void supersampler::add_sample(colour const& colour) {
    ++count;
    r += colour.red();
    g += colour.green();
    b += colour.blue();
}

colour supersampler::value() const {
    if (count == 0) return colour{0};
    else return {
        r / count,
        g / count,
        b / count
    };
}
