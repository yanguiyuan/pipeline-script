use regex::Regex;
use std::fs::read_to_string;

pub trait Preprocessor {
    fn process(&mut self, script: &str) -> String;
}
#[derive(Default)]
pub struct FormatStringPreprocessor;
impl Preprocessor for FormatStringPreprocessor {
    fn process(&mut self, script: &str) -> String {
        let re = Regex::new(r#"f"(.*?)""#).unwrap();
        let replace = re.replace_all(script, |cap: &regex::Captures| {
            let mat = cap.get(1).unwrap();
            let s = mat.as_str();
            let replace = s.replace("{", r#"","#).replace("}", r#",""#);
            format!(r#"FormatAppend("{replace}")"#)
        });
        replace.to_string()
    }
}
#[derive(Default)]
pub struct ImportPreprocessor;

impl ImportPreprocessor {
    fn helper(imports: &mut Vec<String>, script: impl AsRef<str>) -> (String, bool) {
        let origin = script.as_ref();
        let re = Regex::new(r"\bimport\b\s+([a-zA-Z_][a-zA-Z0-9_]*)\s+").unwrap();
        let replace = re.replace_all(origin, |cap: &regex::Captures| {
            let mat = cap.get(1).unwrap();
            let s = mat.as_str().into();
            if imports.contains(&s) {
                return String::new();
            }
            imports.push(s.clone());
            read_to_string(format!("{s}.ppl")).unwrap()
        });
        (replace.to_string(), re.is_match(origin))
    }
}
impl Preprocessor for ImportPreprocessor {
    fn process(&mut self, script: &str) -> String {
        let mut s = script.into();
        let mut imports = vec![];
        loop {
            let (str, b) = Self::helper(&mut imports, s);
            s = str;
            if !b {
                break;
            }
        }
        s
    }
}
