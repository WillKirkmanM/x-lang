import std::print;

fn hanoi(n, source, auxiliary, target) {
    if n > 0 {
        hanoi(n - 1, source, target, auxiliary);
        
        print("Move disk {n} from {source} to {target}");
        
        hanoi(n - 1, auxiliary, source, target);
    }
}

print("Tower of Hanoi solution with 3 disks:");
hanoi(3, "A", "B", "C");