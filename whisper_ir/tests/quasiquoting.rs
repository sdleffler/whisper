use whisper_ir::{IrGoal, IrNode, IrRelation};

whisper_derive::relation! {
    fn quasiquoted_relation(goal: IrGoal, term: IrNode);

    foo if X #term Y,
        #goal;
}

whisper_derive::module! {
    fn make_simple(rel: IrRelation);

    trude is_mother_of sally;

    tom  is_father_of sally;
    tom  is_father_of erica;
    mike is_father_of tom;

    X is_sibling_of Y if Z is_parent_of X, Z is_parent_of Y;

    X is_parent_of Y if X is_father_of Y;
    #rel
}

whisper_derive::query! {
    fn make_query(person_a: IrNode, person_b: IrNode);

    #person_a is_sibling_of #person_b;
}
