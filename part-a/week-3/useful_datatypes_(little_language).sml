datatype expression = Constant of int
		    | Negate of expression
		    | Add of expression * expression
		    | Multiply of expression * expression;

fun eval exp =
    case exp of
	Constant i => i
      | Negate e2 => ~ (eval e2)
      | Add (e1, e2) => (eval e1) + (eval e2)
      | Multiply (e1, e2) => (eval e1) * (eval e2);


val example_expression = Multiply (Add (Constant 6, Constant 3), Constant 10);

val example_answer_from_eval = eval example_expression;

fun number_of_adds exp =
    case exp of
	Constant i => 0
      | Negate e1 => number_of_adds (e1)
      | Add (e1, e2) => 1 + number_of_adds (e1) + number_of_adds (e2)
      | Multiply (e1, e2) => number_of_adds (e1) + number_of_adds (e2);

