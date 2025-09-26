use doublefmt::format_file;

fn main() {
    let mut count = Some(5);
    let output = format_file!("examples/file.txt", name = "Alice", count = count);

    println!("{}", output);

    count = None;
    let output = format_file!("examples/file.txt", name = "Alice", count = count);

    println!("{}", output);
}
