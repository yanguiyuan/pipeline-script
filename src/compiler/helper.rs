use crate::ast::r#type::Type;

pub(crate) fn check_type_match(ty1: &Type, ty2: &Type) -> bool {
    if ty1 == ty2 {
        return true;
    }
    if ty1.is_i8() && ty2.is_alias() {
        return true;
    }
    if ty1.is_ref() && ty2.is_ref() {
        return check_type_match(&ty1.unwrap_ref(), &ty2.unwrap_ref());
    }
    if ty1.is_struct() && ty2.is_struct() {
        return ty1.get_struct_name().unwrap() == ty2.get_struct_name().unwrap();
    }
    if let Type::GenericInstance { instance, .. } = ty1 {
        return check_type_match(&**instance, ty2);
    }
    if let Type::GenericInstance { instance, .. } = ty2 {
        return check_type_match(ty1, &**instance);
    }
    false
}
