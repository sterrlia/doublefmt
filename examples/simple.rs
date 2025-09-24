use doublefmt::doublefmt;

fn main() {
    let output = doublefmt!("Hello {{name}}, you are {{age}}!", name = "Alice", age = 34);
    println!("{}", output);
}

