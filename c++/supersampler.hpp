#pragma once

#include "colour.hpp"

class supersampler final {
    int count;
    double r;
    double g;
    double b;
public:
    supersampler();
    void add_sample(colour colour);
    colour value() const;
};
