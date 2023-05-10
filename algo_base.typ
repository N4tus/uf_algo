#import "types.typ": *
#import "style_base.typ": eval_line

#let _ex_format(name, fn) = makeup => {
  let format = if t_check(TMakeup, makeup) {
    makeup.format 
  } else {
    makeup
  }
  if name == none or name in format {
    fn(format)
  } else {
    panic("formatter '" + repr(format) + "' is missing '" + name + "' field")
  }
}

#let block(body, indent) = {
  (body: body, indent: indent)
}

#let _choose_formatter(format, default, name) = {
  if name in format {
    format.at(name)
  } else {
    default
  }
}

#let _line_format(line_part, format: none) = {
  if type(line_part) == "array" {
    line_part.flatten().map(part => if type(part) == "function" { part(format) } else { part })
  } else if type(line_part) == "function" {
    line_part(format)
  } else {
    line_part
  }
}

#let constant(name) = _ex_format(name, format => format.at(name))

#let expr(name, ..values, format: none, format_name: "format") = _ex_format(name, _format => {
  let values = values.pos()
  let default_format = if format != none { format } else { (..v)=>v.pos().flatten().map(v => if t_check(TNumber, v) { str(v) } else {v}) }
  let formatter = _choose_formatter(_format.at(name), default_format, format_name)
  formatter(..values.map(_line_format.with(format: _format)))
})

#let control_flow(name, ..values, format_head: none, format_tail: none, format_head_name: "format_head", format_tail_name: "format_tail", indent: 1em, indent_name: "indent", with_body: d=>d) = (..body) => {
  (
    expr(name, ..values, format: format_head, format_name: format_head_name),
    block(with_body(body.pos()), _ex_format(name, format => _choose_formatter(format.at(name), indent, indent_name))), 
    expr(name, ..values, format: format_tail, format_name: format_tail_name),
  )
}

#let _mk_array(e) = if type(e) == "array" {e} else {(e,)}

#let append_content(before: (), after: ()) = content => {
  _mk_array(before) + _mk_array(content) + _mk_array(after)
}

#let algo_body(makeup) = (..content) => {
  t_assert(TMakeup, makeup)

  let resolve_groups(elem, start_idx) = {
    let i = start_idx
    let items = ()
    while i < elem.len() {
      let e = elem.at(i)
      if t_check(TGroupStart, e) {
        let (inner, next_idx) = resolve_groups(elem, i+1)
        i = next_idx

        let start_style = if "start_style" in e { e.start_style } else { v=>v }
        let end_style   = if "end_style"   in e { e.end_style }   else { v=>v }
        let use_start   = if "use_start"   in e { e.use_start }   else { v=>v }
        let use_end     = if "use_end"     in e { e.use_end }     else { v=>v }

        let item = if use_start and use_end {
          (e.group)(inner, start_style, end_style)
        } else if use_start {
          (e.group)(inner, start_style)
        } else if use_end {
          (e.group)(inner, end_style)
        } else {
          (e.group)(inner)
        }
        
        items.push(item)
      } else if t_check(TGroupEnd, e) {
        return (items.join(""), i+1)
      } else {
        items.push(e)
        i = i+1
      }
    }
    (items.join(""), i)
  }
  let indent(lines, level, makeup) = {
    for l in lines {
      if t_check(TBody, l) {
        indent(l.body, level + (l.indent)(makeup), if "body" in makeup { format.body } else { makeup })
      } else if type(l) == "array" {
        indent(l, level, makeup)
      } else {
        let elem = l(makeup)
        if elem == none { continue }
        let elem = if type(elem) == "array" {
          resolve_groups(eval_line(elem, makeup), 0).at(0)
        } else {
          elem
        }
        (h(level) + elem,)
      }
    }
  }
  indent(content.pos(), 0em, makeup)
}

#let algo(name, makeup) = (..content) => {
  t_assert(TMakeup, makeup)
  figure(align(
    start,
    [
      #box(line(length: 100%))
      #name
      #box(line(length: 100%))
      #enum(..algo_body(makeup)(..content), numbering: "1:")
      #box(line(length: 100%))
    ]
  ))
}
