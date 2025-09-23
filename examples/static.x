fn process(static config: str, data: i32) -> i32 {
    if config == "fast" {
        return data * 2;
    } else if config == "precise" {
        return data * 4;
    } else {
        return data; // Default path
    }
}

fn main() {
    let result1 = process("fast", 100);     // Calls specialised `process_spec_fast`
    let result2 = process("precise", 50);   // Calls specialised `process_spec_precise`
}