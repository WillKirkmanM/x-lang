fn hanoi(n: f64, source: str, auxiliary: str, target: str) {
    if n > 0.0 {
        hanoi(n - 1.0, source, target, auxiliary);
        
        print("Move disk {n} from {source} to {target}");
        
        hanoi(n - 1.0, auxiliary, source, target);
    }
}

print_str("Tower of Hanoi solution with 3 disks:");
hanoi(3.0, "A", "B", "C");