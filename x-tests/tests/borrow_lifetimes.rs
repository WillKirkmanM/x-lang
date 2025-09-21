use x_borrow_checker::BorrowChecker;
use x_parser::parse;
use x_typechecker::TypeChecker;

fn typecheck(src: &str) -> x_ast::Program {
    let prog = parse(src).expect("parse failed");
    TypeChecker::new().check(prog).expect("typecheck failed")
}

#[test]
fn returns_ref_bad_is_rejected() {
    let src = r#"
        fn returns_ref_bad<'a>() -> &'a f64 {
            let v: f64 = 42.0;
            let r: &'a f64 = &v;
            return r;
        }
    "#;

    let typed = typecheck(src);
    let mut bc = BorrowChecker::new();
    let res = bc.check(&typed);
    assert!(
        res.is_err(),
        "expected borrow checker to reject returning local reference"
    );
    let err = res.unwrap_err();
    assert!(
        err.contains("Cannot return a reference value"),
        "unexpected error: {}",
        err
    );
}

#[test]
fn mixed_borrows_conflict_is_rejected() {
    let src = r#"
        struct Vec2 { x: f64, y: f64 }
        fn mixed_borrows_conflict() {
            let mut v: Vec2 = Vec2 { x: 1.0, y: 2.0 };
            let rx: &f64 = &v.x;
            let mx: &mut f64 = &mut v.x;
        }
    "#;

    let typed = typecheck(src);
    let mut bc = BorrowChecker::new();
    let res = bc.check(&typed);
    assert!(
        res.is_err(),
        "expected borrow checker to reject conflicting borrows"
    );
    let err = res.unwrap_err();
    assert!(
        err.contains("Cannot mutably borrow") || err.contains("already borrowed"),
        "unexpected error: {}",
        err
    );
}

#[test]
fn lifetime_escape_is_rejected() {
    let src = r#"
        fn lifetime_escape() {
            let r: &f64;
            {
                let tmp: f64 = 3.14;
                r = &tmp;
            }
        }
    "#;

    let typed = typecheck(src);
    let mut bc = BorrowChecker::new();
    let res = bc.check(&typed);
    assert!(
        res.is_err(),
        "expected borrow checker to reject escaping reference"
    );
    let err = res.unwrap_err();
    assert!(
        err.contains("Reference does not live long enough")
            || err.contains("does not live long enough"),
        "unexpected error: {}",
        err
    );
}

#[test]
fn ok_block_is_accepted() {
    let src = r#"
        struct Vec2 { x: f64, y: f64 }
        fn ok_block() {
            let mut v: Vec2 = Vec2 { x: 3.0, y: 4.0 };
            let px: &mut f64 = &mut v.x;
        }
    "#;

    let typed = typecheck(src);
    let mut bc = BorrowChecker::new();
    let res = bc.check(&typed);
    assert!(
        res.is_ok(),
        "expected borrow checker to accept ok_block, got: {:?}",
        res
    );
}
