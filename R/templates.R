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
{{#hashaving}}
group by {{{table}}}.{{{to}}}
having {{{having}}}
{{/hashaving}}
"
}

lb_compound_template <- function() {
"({{{block1}}}
{{{operator}}}
{{{block2}}})"
}

aggregate_q_template <- function() {
"
select {{{table}}}.{{{id_field}}} as {{{id_type}}}
from {{{schema}}}.{{{table}}}
{{#haswhere}}
where {{{where}}}
{{/haswhere}}
{{#hashaving}}
group by {{{table}}}.{{{id_field}}}
having {{{having}}}
{{/hashaving}}
"
}

custom_q_template <- function() {
"
select {{{id_field}}} as {{{id_type}}}
from ({{{custom}}})
{{#haswhere}}
where {{{where}}}
{{/haswhere}}
{{#hashaving}}
group by {{{id_field}}}
having {{{having}}}
{{/hashaving}}
"
}
