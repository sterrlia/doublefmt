use doublefmt::format_file;

fn main() {
    let output = format_file!(
        "examples/file.txt",
        name = "Alice",
        count = 5
    );

    println!("{}", output);
}

