fn trunc(n: f64) -> f64 {
  return n - (n % 1.0);
}

fn binary_search(arr: [f64], needle: f64) -> f64 {
    let mut left: f64 = 0.0;
    let mut right: f64 = 4.0;

    while left <= right {
        let mid_float: f64 = left + (right - left) / 2.0;
        
        let mid: f64 = trunc(mid_float); 
        
        if arr[mid] == needle {
            return mid;
        }
        
        if arr[mid] < needle {
            left = mid + 1.0;
        } else {
            right = mid - 1.0;
        }
    }
    
    return -1.0; 
}
let sorted_array: [f64] = [10.0, 20.0, 30.0, 40.0, 50.0];

print_str("Binary search examples:");
print_str("Searching for 30:");
let index: f64 = binary_search(&sorted_array, 30.0);
print_str("Found at index:");
print(index);

print_str("Searching for 20:");
let index2: f64 = binary_search(&sorted_array, 20.0);
print_str("Found at index:");
print(index2);