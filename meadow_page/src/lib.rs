use std::path::PathBuf;
use crate::meadowfile::Meadowfile;

#[rustler::nif]
pub fn read_meadowfile(path: &str) -> Result<(), Box<dyn std::error::Error>> {
    let meadowfile = Meadowfile::from_path(&Path::new(path))?;
    for line in &meadowfile.lines {
        println!("Line: {:?}", line);
    }

    Ok(())
}

rustler::init!("meadow_page", [read_meadowfile]);

#[cfg(test)]
mod tests {
    use super::*;

    // Add your test cases here
}
