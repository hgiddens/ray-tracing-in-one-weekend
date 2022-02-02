mod colour;
use crate::colour::Colour;

fn main() {
    let cols = 200;
    let rows = 100;
    println!("P3");
    println!("{} {}", cols, rows);
    println!("255");

    for row in (0..rows).rev() {
        for col in 0..cols {
            let colour = Colour::new(
                row as f32 / rows as f32,
                col as f32 / cols as f32,
                0.2
            );
            let (ir, ig, ib) = colour.rgb8();
            println!("{} {} {}", ir, ig, ib);
        }
    }
}
