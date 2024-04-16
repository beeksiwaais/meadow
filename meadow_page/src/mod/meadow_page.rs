
struct MeadowPage {
    pub version: String,
    pub created_at: String,
    pub lines: Vec<Line>,
    #[cfg(feature = "scriptable")]
    pub scripts: Vec<Script>,
}

enum LineType {
   Text,
   Image,
   Link,
   ScriptOutput,
}

struct Line {
   pub id: i32,
   pub line_type: LineType,
   // pub is_hidden: bool, not need because user need to add manually file and folder they want to show 
   pub previous_id: Option<i32>,
}

impl MeadowPage {
   pub fn from_path(path: &str) -> Database {
       let file = File::open(path)?;
       let metadata = file.metadata()?;
       let created_at = metadata.created()?;

       // read contents with BufReader and Bison
       let reader = BufReader::new(file);
       
       for value in buffer {
           println!("BYTE: {}", value);
       }
       
       Ok(
           MeadowPage {
               created_at,
           }
       )
   }
}