import std::print;

fn selection_sort(arr) {
    let n = 5;
    let i = 0;
    
    while i < n - 1 {
        let min_idx = i;
        let j = i + 1;
        
        while j < n {
            if arr[j] < arr[min_idx] {
                min_idx = j;
            }
            j = j + 1;
        }
        
        if min_idx != i {
            let temp = arr[min_idx];
            arr[min_idx] = arr[i];
            arr[i] = temp;
        }
        
        i = i + 1;
    }
}

fn print_array(arr) {
    let i = 0;
    while i < 5 {
        print(arr[i]);
        i = i + 1;
    }
}

let numbers = [5, 3, 8, 1, 2];

print("Original array:");
print_array(numbers);

selection_sort(numbers);

print("Sorted array:");
print_array(numbers);