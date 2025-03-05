import std::print;

fn binary_search(arr, needle) {
    let left = 0;
    let right = 5 - 1;
    
    while left <= right {
        let mid = left + (right - left) / 2;
        
        if arr[mid] == needle {
            return mid;
        }
        
        if arr[mid] < needle {
            left = mid + 1;
        } else {
            right = mid - 1;
        }
    }
    
    return -1; 
}

let sorted_array = [10, 20, 30, 40, 50];

print("Binary search examples:");
print("Searching for 30:");
let index = binary_search(sorted_array, 30);
print("Found at index:");
print(index);

print("Searching for 20:");
index = binary_search(sorted_array, 20);
print("Found at index:");
print(index);