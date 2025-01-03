use std::ops::{Add, AddAssign};
use std::string::ToString;

#[derive(Debug, Clone)]
pub struct Position {
    pub(crate) pos: usize,
    #[allow(unused)]
    pub(crate) row: usize,
    #[allow(unused)]
    pub(crate) col: usize,
    pub(crate) span: usize,
    pub module_name: String,
}

impl Add for Position {
    type Output = Position;

    fn add(self, rhs: Self) -> Self::Output {
        let mut pos = self.clone();
        pos.span = rhs.pos.abs_diff(self.pos) + rhs.span;
        pos
    }
}
impl AddAssign for Position {
    fn add_assign(&mut self, rhs: Self) {
        self.span = rhs.pos.abs_diff(self.pos) + rhs.span;
    }
}
impl Position {
    pub fn none() -> Self {
        Self {
            pos: 0,
            span: 0,
            row: 0,
            col: 0,
            module_name: "None".to_string(),
        }
    }
    pub fn new(pos: usize, span: usize, row: usize, col: usize, module_name: &str) -> Self {
        Self {
            pos,
            span,
            row,
            col,
            module_name: module_name.into(),
        }
    }
    #[allow(unused)]
    pub fn set_span(&mut self, span: usize) {
        self.span = span;
    }
    #[allow(unused)]
    pub fn set_pos(&mut self, pos: usize) {
        self.pos = pos;
    }
    #[allow(unused)]
    pub fn add_span(&mut self, add: usize) {
        self.span += add
    }
    #[allow(unused)]
    pub fn is_none(&self) -> bool {
        if self.pos == 0 && self.span == 0 {
            return true;
        }
        false
    }
    #[allow(unused)]
    pub fn get_raw_string(&self, source: &[char]) -> String {
        let utf8_bytes = source[self.pos..self.pos + self.span]
            .iter()
            .flat_map(|&c| c.encode_utf8(&mut [0; 4]).bytes().collect::<Vec<_>>())
            .collect::<Vec<_>>();
        String::from_utf8(utf8_bytes).unwrap()
    }
    #[allow(unused)]
    pub fn get_row_col(&self, source: &[char]) -> (usize, usize) {
        let mut row = 0;
        let mut col = 0;
        for (i, &ch) in source.iter().enumerate() {
            // 如果已经到达或超过pos指定的位置，则退出循环
            if i >= self.pos {
                break;
            }
            // 如果遇到换行符，则行数增加，列数重置（除非换行符是最后一个字符）
            if ch == '\n' {
                row += 1;
                col = 0;
            } else {
                col += 1;
            }
        }
        // 由于循环在到达pos时退出，我们需要检查最后一个字符是否是换行符
        // 如果是，则实际的列数应该是换行符之前的列数
        if let Some(&last_char) = source.get(self.pos - 1) {
            if last_char == '\n' {
                col = 0;
            }
        }
        // 返回计算出的行和列
        (row, col)
    }
}
