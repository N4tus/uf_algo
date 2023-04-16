#import "algo_base.typ": control_flow, expr, constant, algo, append_content
#let For(vars, container) = control_flow("for",    vars, container)
#let While( condition)    = control_flow("while",  condition)
#let If(    condition)    = control_flow("if",     condition)
#let Switch(condition)    = control_flow("switch", condition)
#let Case(  condition)    = control_flow("case",   condition, with_body: append_content(after: expr("case", format_name: "break")))

#let Fn(name, param, ret) = control_flow("function", name, param, ret)

#let Assign(name, content) = expr("assign",    name, content)
#let Return(content)       = expr("return",    content)
#let Break(content)        = expr("break",     content)
#let Continue(content)     = expr("continue",  content)
#let Statement(content)    = expr("statement", content)
#let Comment(comment)      = expr("comment",   comment)

#let FnCall(fn, ..args) = expr("fn_call", fn, args.pos())
#let MethodCall(obj, method, ..args) = expr("method_call", obj, method, args.pos())

#let Str(s)         = expr("str",   s)
#let Num(num)       = expr("num",   num)
#let Ident(ident)   = expr("ident", ident)
#let Tuple(..terms) = expr("tuple", terms.pos())
#let List(..terms)  = expr("list",  terms.pos())
#let Seq(..terms)   = expr("seq",   terms.pos())

#let Eq    = constant("eq")
#let NotEq = constant("not_eq")
#let And   = constant("and")
#let Or    = constant("or")
#let Not   = constant("not")
#let Gt    = constant("gt")
#let Lt    = constant("lt")
#let GtEq  = constant("gteq")
#let LtEq  = constant("lteq")
#let Plus  = constant("plus")
#let Minus = constant("minus")
#let Mul   = constant("mul")
#let Div   = constant("div")
#let Mod   = constant("mod")




