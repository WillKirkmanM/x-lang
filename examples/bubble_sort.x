import std::print;

fn bubble_sort(arr) {
    let n = 5;
    let i = 0;
    
    while i < n {
        let j = 0;
        while j < n - i - 1 {
            if arr[j] > arr[j + 1] {
                let temp = arr[j];
                arr[j] = arr[j + 1];
                arr[j + 1] = temp;
            }
            j = j + 1;
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

bubble_sort(numbers);

print("Sorted array:");
print_array(numbers);