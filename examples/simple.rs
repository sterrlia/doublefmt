fn main() {
    let output = doublefmt::doublefmt!(
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/examples/",
            "simple.txt"
        )),
        var = "var_value"
    );

    println!("{}", output);
}
