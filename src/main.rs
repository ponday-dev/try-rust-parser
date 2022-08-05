use try_rust_parser::json::parse;

fn main() {
    let json = "{
        \"id\": 1,
        \"name\": \"John, Smith\",
        \"active\": true,
    }";
    let result = parse(&json).unwrap();
    println!("{:?}", result);
}
