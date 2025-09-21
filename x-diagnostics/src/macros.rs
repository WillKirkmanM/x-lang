#[macro_export]
macro_rules! diag_error {
    ($span:expr, $($fmt:tt)*) => {{
        $crate::Diagnostic::error(format!($($fmt)*))
            .with_label($crate::Label::primary($span, "here"))
    }};
}
