use crate::parser::declaration::VariableDeclaration;
use crate::parser::r#type::Type;
use crate::parser::stmt::Stmt;

use super::stmt::StmtNode;
#[derive(Clone, Debug)]

pub struct Function {
    name: String,
    return_type: Type,
    args: Vec<VariableDeclaration>,
    body: Vec<StmtNode>,
    is_generic: bool,
    binding_struct: Option<String>,
    #[allow(unused)]
    pub(crate) is_extern: bool,
}
impl Function {
    pub fn new(
        name: String,
        return_type: Type,
        args: Vec<VariableDeclaration>,
        body: Vec<StmtNode>,
        is_extern: bool,
    ) -> Self {
        Self {
            name,
            return_type,
            args,
            body,
            binding_struct: None,
            is_generic: false,
            is_extern,
        }
    }
    pub fn name(&self) -> String {
        self.name.clone()
    }
    pub fn body(&self) -> &Vec<StmtNode> {
        &self.body
    }
    pub fn args(&self) -> &Vec<VariableDeclaration> {
        &self.args
    }
    pub fn return_type(&self) -> &Type {
        &self.return_type
    }
    pub fn with_extern(mut self, is_extern: bool) ->Self {
        self.is_extern = is_extern;
        self

    }
    pub fn args_count(&self) -> usize {
        self.args.len()
    }
    pub fn has_binding(&self)->bool{
        match self.binding_struct {
            None => false,
            Some(_) => true
        }
    }
    pub fn get_binding(&self)->String{
        self.binding_struct.clone().unwrap()
    }
    pub fn insert_arg(&mut self,index:usize,vd:VariableDeclaration){
        self.args.insert(index,vd)
    }
    pub fn set_binding_struct(&mut self, binding_struct: impl Into<String>) {
        self.binding_struct = Some(binding_struct.into());
    }

    pub fn set_body(&mut self, body: Vec<StmtNode>) {
        self.body = body;
    }
    pub fn with_return_type(mut self, return_type: Type)->Self {
        self.return_type = return_type;
        self
    }
    pub fn set_return_type(&mut self, return_type: Type) {
        self.return_type = return_type;
    }
    pub fn with_name(mut self, name: String)->Self {
        self.name = name;
        self
    }
    pub fn with_args(mut self, args: Vec<VariableDeclaration>)->Self {
        self.args = args;
        self
    }
    pub fn get_param_type(&self, index: usize) -> Option<Type> {
        if index < self.args.len() {
            self.args[index].r#type()
        } else {
            None
        }
    }
    pub fn get_type(&self) -> Type {
        let mut args = vec![];
        for i in &self.args{
            args.push(i.r#type().unwrap());
        }
        Type::Function(Box::new(self.return_type.clone()), args)
    }

}

impl Default for Function {
    fn default() -> Self {
        Self {
            name: "".to_string(),
            return_type: Type::Unit,
            is_generic: false,
            args: vec![],
            binding_struct: None,
            body: vec![],
            is_extern: false,
        }
    }
}