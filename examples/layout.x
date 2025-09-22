#[layout(SoA)]
struct Particle {
    x: f64,
    y: f64,
    vx: f64,
    mass: f64,
}

fn update(particles: &mut [Particle], dt: f64) {
    for i in 0..1024 {
        // This access pattern is now highly efficient
        particles[i].x = particles[i].x + particles[i].vx * dt;
    }
}

fn main() {}
