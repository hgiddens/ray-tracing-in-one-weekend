#[derive(Clone, Copy, Debug)]
pub struct Colour {
    r: f32,
    g: f32,
    b: f32,
}

impl Colour {
    // TODO: Way of constraining f to be [0, 1]?
    pub fn blend(from: Colour, to: Colour, t: f32) -> Self {
        let u = 1.0 - t;
        Colour {
            r: u * from.r + t * to.r,
            g: u * from.g + t * to.g,
            b: u * from.b + t * to.b,
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

    pub fn rgb8(self) -> (u8, u8, u8) {
        (
            (self.r * 255.9f32) as u8,
            (self.g * 255.9f32) as u8,
            (self.b * 255.9f32) as u8,
        )
    }
}
