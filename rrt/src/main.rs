fn main() {
    let cols = 200;
    let rows = 100;
    println!("P3");
    println!("{} {}", cols, rows);
    println!("255");

    for row in (0..rows).rev() {
        for col in 0..cols {
            let r = row as f32 / rows as f32;
            let g = col as f32 / cols as f32;
            let b = 0.2f32;

            let ir = (255.9 * r) as i32;
            let ig = (255.9 * g) as i32;
            let ib = (255.9 * b) as i32;

            println!("{} {} {}", ir, ig, ib);
        }
    }
}
