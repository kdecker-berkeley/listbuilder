flist_template <- function() {
"
select {{{table}}}.{{{to}}} as {{{id_type}}}
from {{{schema}}}.{{{table}}}
where
{{#haswhere}}
{{{where}}}
and
{{/haswhere}}
{{{from}}} in ({{{original_query}}})
"
}

lb_compound_template <- function() {
"({{{block1}}})
{{{operator}}}
({{{block2}}})"
}

simple_q_template <- function() {
"select {{{table}}}.{{{id_field}}} as {{{id_type}}}
from {{{schema}}}.{{{table}}}
{{#haswhere}}
where {{{where}}}
{{/haswhere}}"
}
