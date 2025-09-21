fn selection_sort(mut arr: [i32]) -> [i32] {
    let n: i32 = 5;
    let mut i: i32 = 0;
    
    while i < n - 1 {
        let mut min_idx: i32 = i;
        let mut j: i32 = i + 1;
        
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

    arr
}

fn print_array(arr: [i32]) {
    let mut i: i32 = 0;
    while i < 5 {
        let number_at_index: f64 = arr[i];
        print(arr[i] );
        print(number_at_index);
        i = i + 1;
    }
}


fn main() {
    let numbers: [i32] = [5, 3, 8, 1, 2];
    
    print_str("Original array:");
    print_array(&numbers);
    
    let numbers = selection_sort(numbers);
    
    print_str("Sorted array:");
    print_array(&numbers);
}