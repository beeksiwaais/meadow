use std::path::Path;

#[rustler::nif]
pub fn create_meadowfile(path: String) -> bool {
    let path_obj = Path::new(&path);
    if !path_obj.is_dir() {
        return false;
    }

    let path_obj = path_obj.join(".meadowfile");
    if path_obj.exists() {
        return false;
    }

    // V1 Spec SQL
    let query = "
    CREATE TABLE info (version VARCHAR(255), created_at TIMESTAMP);
    INSERT INTO info VALUES ('1.0', now());
    CREATE TABLE line (
        id INTEGER PRIMARY KEY, 
        entry_type VARCHAR(255), 
        previous_id INTEGER NULL,
        FOREIGN KEY(previous_id) REFERENCES line(id)
    );
    ";

    let connection = sqlite::open(path)
        .expect("Failed to open connection");

    let res = connection.execute(query).is_ok();
    
    return res;
}

//#[cfg(test)]
//mod tests {
//    use super::*;
//
 //   #[test]
  //  fn it_works() {
  //      let result = add(2, 2);
  //      assert_eq!(result, 4);
  //  }
//}
