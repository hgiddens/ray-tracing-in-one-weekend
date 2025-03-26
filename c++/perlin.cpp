#include "perlin.hpp"

#include <cmath>
// I am too tired to make this use the C++ random stuff.
#include <cstdlib>

namespace {
    std::vector<double> perlin_generate() {
        std::vector<double> p(256);
        for (size_t i = 0; i < p.size(); ++i) {
            p[i] = drand48();
        }
        return p;
    }

    template<typename T> void permute(std::vector<T>& p) {
        for (size_t i = p.size() - 1; i > 0; --i) {
            size_t const target = int(drand48() * (i + 1));
            std::swap(p[i], p[target]);
        }
    }

    std::vector<uint8_t> perlin_generate_perm() {
        std::vector<uint8_t> p(256);
        // TODO: iota
        for (size_t i = 0; i < p.size(); ++i) {
            p[i] = i;
        }
        permute(p);
        return p;
    }

    double trilinear_interpolation(double const c[2][2][2], double const u, double const v, double const w) {
        double accum = 0;
        for (int i = 0; i < 2; ++i) {
            for (int j = 0; j < 2; ++j) {
                for (int k = 0; k < 2; ++k) {
                    accum +=
                        (i*u + (1-i)*(1-u)) *
                        (j*v + (1-j)*(1-v)) *
                        (k*w + (1-k)*(1-w)) *
                        c[i][j][k];
                }
            }
        }
        return accum;
    }
}

std::vector<double> const perlin::ranfloat = perlin_generate();
std::vector<uint8_t> const perlin::perm_x = perlin_generate_perm();
std::vector<uint8_t> const perlin::perm_y = perlin_generate_perm();
std::vector<uint8_t> const perlin::perm_z = perlin_generate_perm();

double perlin::noise(vec3 const& p) const {
    double const
        u = p.x() - std::floor(p.x()),
        v = p.y() - std::floor(p.y()),
        w = p.z() - std::floor(p.z());
    int const
        i = std::floor(p.x()),
        j = std::floor(p.y()),
        k = std::floor(p.z());
    double c[2][2][2];
    for (int di = 0; di < 2; ++di) {
        for (int dj = 0; dj < 2; ++dj) {
            for (int dk = 0; dk < 2; ++dk) {
                c[di][dj][dk] = ranfloat[perm_x[(i+di)&255] ^ perm_y[(j+dj)&255] ^ perm_z[(k+dk)&255]];
            }
        }
    }
    return trilinear_interpolation(c, u, v, w);
}
