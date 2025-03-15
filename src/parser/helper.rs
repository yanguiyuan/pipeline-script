use crate::context::Context;

pub fn is_enum(ctx: &Context, id: &str) -> bool {
    ctx.get_type_alias(id)
        .map_or(false, |ty| ty.get_type().is_enum())
}
