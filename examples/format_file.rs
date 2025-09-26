use doublefmt::format_file;

fn main() {
    let mut count = Some(5);
    let mut name = Some("Alice".to_string());
    let output = format_file!("examples/file.txt", name = name, count = count);

    println!("{}", output);

    name = None;
    count = Some(3);
    let output = format_file!("examples/file.txt", name = name, count = count);

    println!("{}", output);
}
