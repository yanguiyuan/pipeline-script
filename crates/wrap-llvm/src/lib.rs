pub mod context;
pub mod module;
pub mod types;
mod executor;
pub mod builder;
pub mod function;
pub mod value;
pub mod global;

pub fn add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
