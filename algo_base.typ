#let _ex_style(name, fn) = style => {
  if name == none or name in style {
    fn(style)
  } else {
    panic(repr(style) + " is missing '" + name + "' field")
  }
}

#let block(body) = {
  (body: body, indent: 1em)
}

#let _choose_format(style, default, name) = {
  if name in style {
    style.at(name)
  } else {
    default
  }
}

#let _line_format(line_part, style: none) = {
  if type(line_part) == "array" {
    line_part.flatten().map(part => if type(part) == "function" { part(style) } else { part })
  } else if type(line_part) == "function" {
    line_part(style)
  } else {
    line_part
  }
}

#let constant(name) = _ex_style(name, style => style.at(name))

#let _format_fns = (
  ()=>none,
  (a1)=>a1,
  (a1, a2)=>a1+a2,
  (a1, a2, a3)=>a1+a2+a3,
  (a1, a2, a3, a4)=>a1+a2+a3+a4,
  (a1, a2, a3, a4, a5)=>a1+a2+a3+a4+a5,
  (a1, a2, a3, a4, a5, a6)=>a1+a2+a3+a4+a5+a6,
)
#let expr(name, ..values, format: none, format_name: "format") = _ex_style(name, style => {
  let values = values.pos()
  let default_format = if format != none { format } else { _format_fns.at(values.len()) }
  let format = _choose_format(style.at(name), default_format, format_name)
  format(..values.map(_line_format.with(style: style)))
})

#let control_flow(name, ..values, format_head: none, format_tail: none, format_head_name: "format_head", format_tail_name: "format_tail", with_body: d=>d) = (..body) => {
  (
    expr(name, ..values, format: format_head, format_name: format_head_name),
    block(with_body(body.pos())), 
    expr(name, ..values, format: format_tail, format_name: format_tail_name),
  )
}

#let _mk_array(e) = if type(e) == "array" {e} else {(e,)}

#let append_content(before: (), after: ()) = content => {
  _mk_array(before) + _mk_array(content) + _mk_array(after)
}

#let eval_line(elem, style) = elem.flatten().map(e => 
  if type(e) == "dictionary" { 
    let first_style = e.styles.at(0)
    let ex_prec = first_style.precedence
    let acc = (fn: first_style.fn, prec: ex_prec(style))
    let chosen_style = e.styles.slice(1).fold(acc, (acc, style_def) => {
      let ex_precedence = style_def.precedence
      let precedence = ex_precedence(style)
      if precedence > acc.prec {
        (fn: style_def.fn, prec: precedence)
      } else {
        acc
      }
    })
    let styler = chosen_style.fn
    styler(e.value, style)
  } else { e }
).join("")

#let algo_body(format) = (..content) => {
  if not "style" in format {
    panic("Format does not have style field. You have to manually insert the style in the format.")
  }
  let is_body(var) = type(var) == "dictionary" and var.len() == 2 and "body" in var and "indent" in var
  let indent(lines, level, format) = {
    for l in lines {
      if is_body(l) {
        indent(l.body, level + l.indent, if "body" in format { format.body } else { format })
      } else if type(l) == "array" {
        indent(l, level, format)
      } else {
        let elem = l(format)
        if elem == none { continue }
        let elem = if type(elem) == "array" {
          eval_line(elem, format.style)
        } else {
          elem
        }
        (h(level) + elem,)
      }
    }
  }
  indent(content.pos(), 0em, format)
}

#let algo(name, format) = (..content) => {
  figure(align(
    start,
    [
      #box(line(length: 100%))
      #name
      #box(line(length: 100%))
      #enum(..algo_body(format)(..content), numbering: "1:")
      #box(line(length: 100%))
    ]
  ))
}
