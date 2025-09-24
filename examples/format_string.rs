use doublefmt::format_str;

fn main() {
    let output = format_str!("some text with variable: {{ var }}", var = "world");

    println!("{}", output);
}
