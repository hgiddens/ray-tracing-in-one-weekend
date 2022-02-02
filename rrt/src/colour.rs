#[derive(Debug)]
pub struct Colour {
    r: f32,
    g: f32,
    b: f32
}

impl Colour {
    pub fn new(r: f32, g: f32, b: f32) -> Self {
        Colour{ r: r, g: g, b: b }
    }
    
    pub fn rgb8(self) -> (u8, u8, u8) {
        ((self.r * 255.9f32) as u8,
         (self.g * 255.9f32) as u8,
         (self.b * 255.9f32) as u8)
    }
}
