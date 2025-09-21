fn bubble_sort(arr: &mut f64[]) {
    let mut n: f64 = 5.0;
    let mut i: f64 = 0.0;
    
    while i < n {
        let mut j: f64 = 0.0;
        while j < n - i - 1.0 {
            if arr[j] > arr[j + 1.0] {
                let temp: f64 = arr[j];
                arr[j] = arr[j + 1.0];
                arr[j + 1.0] = temp;
            }
            j = j + 1.0;
        }
        i = i + 1.0;
    }
}

fn print_array(arr: &f64[]) {
    let mut i: f64 = 0.0;
    while i < 5.0 {
        print(arr[i]);
        i = i + 1.0;
    }
}

let mut numbers: f64[] = [5.0, 3.0, 8.0, 1.0, 2.0];

print_str("Original array:");
{
    // limit lifetime of this immutable borrow
    print_array(&numbers);
}

bubble_sort(&mut numbers);

print_str("Sorted array:");
print_array(&numbers);