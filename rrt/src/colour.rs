#[derive(Clone, Copy, Debug)]
pub struct Albedo {
    pub r: f32,
    pub g: f32,
    pub b: f32,
}

#[derive(Clone, Copy, Debug)]
pub struct Colour {
    r: f32,
    g: f32,
    b: f32,
}

impl Colour {
    pub fn new(r: f32, g: f32, b: f32) -> Self {
        Colour { r, g, b }
    }

    // TODO: Way of constraining f to be [0, 1]?
    pub fn blend(from: Colour, to: Colour, t: f32) -> Self {
        let u = 1.0 - t;
        Colour {
            r: u * from.r + t * to.r,
            g: u * from.g + t * to.g,
            b: u * from.b + t * to.b,
        }
    }

    pub fn black() -> Self {
        Colour {
            r: 0.0,
            g: 0.0,
            b: 0.0,
        }
    }

    pub fn white() -> Self {
        Colour {
            r: 1.0,
            g: 1.0,
            b: 1.0,
        }
    }

    pub fn light_sky_blue() -> Self {
        Colour {
            r: 0.5294118,
            g: 0.80784315,
            b: 0.98039216,
        }
    }

    pub fn gamma2(self) -> Self {
        Colour {
            r: self.r.sqrt(),
            g: self.g.sqrt(),
            b: self.b.sqrt(),
        }
    }

    pub fn rgb8(self) -> (u8, u8, u8) {
        (
            (self.r * 255.9f32) as u8,
            (self.g * 255.9f32) as u8,
            (self.b * 255.9f32) as u8,
        )
    }
}

// TODO: This is gross and I hate it
impl std::ops::Add for Colour {
    type Output = Self;

    fn add(self, other: Colour) -> Self::Output {
        Colour {
            r: self.r + other.r,
            g: self.g + other.g,
            b: self.b + other.b,
        }
    }
}

// TODO: This is also gross
impl std::ops::Div<i32> for Colour {
    type Output = Self;

    fn div(self, other: i32) -> Self::Output {
        Colour {
            r: self.r / other as f32,
            g: self.g / other as f32,
            b: self.b / other as f32,
        }
    }
}

impl std::ops::Mul<Albedo> for Colour {
    type Output = Self;

    fn mul(self, other: Albedo) -> Self::Output {
        Colour {
            r: self.r * other.r,
            g: self.g * other.g,
            b: self.b * other.b,
        }
    }
}
