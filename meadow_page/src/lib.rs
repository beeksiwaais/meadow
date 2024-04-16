use std::path::{Path, PathBuf};
use crate::meadowfile::Meadowfile;

#[rustler::nif]
pub fn read_meadowfile(path: String) -> bool {
    let meadowfile = Meadowfile::from_path(&PathBuf::from(path));

    match meadowfile {
        Ok(_) => true,
        Err(_) => false,
    }

    meadowfile.lines.iter().for_each(|line| {
        println!("Line: {:?}", line);
    });

}

rustler::init!("lib", [read_meadowfile]);

#[cfg(test)]
mod tests {
    use super::*;
}
