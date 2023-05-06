#import "types-for-typst/types_for_typst.typ": *

#let _ex_style(name, fn) = style => {
  if name == none or name in style {
    fn(style)
  } else {
    panic(repr(style) + " is missing '" + name + "' field")
  }
}

#let block(body, indent) = {
  (body: body, indent: indent)
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
  (a1, a2, a3, a4, a5, a6, a7)=>a1+a2+a3+a4+a5+a6+a7,
  (a1, a2, a3, a4, a5, a6, a7, a8)=>a1+a2+a3+a4+a5+a6+a7+a8,
  (a1, a2, a3, a4, a5, a6, a7, a8, a9)=>a1+a2+a3+a4+a5+a6+a7+a8+a9,
)
#let expr(name, ..values, format: none, format_name: "format") = _ex_style(name, style => {
  let values = values.pos()
  let default_format = if format != none { format } else { _format_fns.at(values.len()) }
  let format = _choose_format(style.at(name), default_format, format_name)
  format(..values.map(_line_format.with(style: style)))
})

#let control_flow(name, ..values, format_head: none, format_tail: none, format_head_name: "format_head", format_tail_name: "format_tail", indent: 1em, indent_name: "indent", with_body: d=>d) = (..body) => {
  (
    expr(name, ..values, format: format_head, format_name: format_head_name),
    block(with_body(body.pos()), style => _choose_format(style.at(name), indent, indent_name)), 
    expr(name, ..values, format: format_tail, format_name: format_tail_name),
  )
}

#let _mk_array(e) = if type(e) == "array" {e} else {(e,)}

#let append_content(before: (), after: ()) = content => {
  _mk_array(before) + _mk_array(content) + _mk_array(after)
}

#let AElem(t_value) = TDict(value: t_value, styles: TArray(TDict(fn: TFunction, precedence: TFunction))) 
#let TElem = AElem(TAny)

#let AGroup(t_group) = TDict(group: t_group, start_style: optional(TFunction), end_style: optional(TFunction), use_start: optional(TBoolean), use_end: optional(TBoolean))
#let TGroup = AGroup(t_or(TFunction, TNone))
#let TGroupElem = AElem(TGroup)
#let TGroupStart = AGroup(TFunction)
#let TGroupEnd = AGroup(TNone)

#let eval_line(elem, style) = elem.flatten().map(e => 
  if t_check(TElem, e) {
    if e.styles.len() == 0 {
      e.value
    } else {
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
      if t_check(TGroupStart, e.value) {
        (start_style: v => styler(v, style), ..e.value)
      } else if t_check(TGroupEnd, e.value) {
        (end_style: v => styler(v, style), ..e.value)
      } else {
        styler(e.value, style)
      }
    }
  } else {
    e
  }
)

#let algo_body(format) = (..content) => {
  if not "style" in format {
    panic("Format does not have style field. You have to manually insert the style in the format.")
  }
  let is_body(var) = type(var) == "dictionary" and var.len() == 2 and "body" in var and "indent" in var and type(var.indent) == "function"
  let gather_group(elem, start_idx) = {
    (none, start_idx)
  }
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
  let indent(lines, level, format) = {
    for l in lines {
      if is_body(l) {
        indent(l.body, level + (l.indent)(format), if "body" in format { format.body } else { format })
      } else if type(l) == "array" {
        indent(l, level, format)
      } else {
        let elem = l(format)
        if elem == none { continue }
        let elem = if type(elem) == "array" {
          resolve_groups(eval_line(elem, format.style), 0).at(0)
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
