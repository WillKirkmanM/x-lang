use x_ast::ExternParam;
use std::io::Write;

pub fn generate_extern_fn_decl<W: Write>(
    writer: &mut W,
    name: &str,
    params: &[ExternParam],
    _return_type: &Option<String>,
) -> std::io::Result<()> {
    write!(writer, "extern ")?;
    
    write!(writer, "{}(", name)?;
    
    for (i, _param) in params.iter().enumerate() {
        if i > 0 {
            write!(writer, ", ")?;
        }
    }
    
    writeln!(writer, ");")?;
    Ok(())
}

pub fn generate_extern_fn_call<W: Write>(
    writer: &mut W,
    name: &str,
    args: &[String],
) -> std::io::Result<()> {
    write!(writer, "{}(", name)?;
    for (i, arg) in args.iter().enumerate() {
        if i > 0 {
            write!(writer, ", ")?;
        }
        write!(writer, "{}", arg)?;
    }
    write!(writer, ")")?;
    Ok(())
}