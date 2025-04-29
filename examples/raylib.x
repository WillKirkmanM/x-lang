extern fn InitWindow(width: i32, height: i32, title: str);
extern fn WindowShouldClose();
extern fn BeginDrawing();
extern fn EndDrawing();
extern fn ClearBackground(color: Color);
extern fn CloseWindow();
extern fn DrawText(text: str, posX: i32, posY: i32, fontSize: i32, color: Color);

struct Color { r, g, b, a }

fn main() {
    let screenWidth = 800;
    let screenHeight = 450;

    InitWindow(screenWidth, screenHeight, "Raylib Window from X!");

    while WindowShouldClose() == 0 {
        BeginDrawing();
        EndDrawing();
    }

    CloseWindow();
}

main();