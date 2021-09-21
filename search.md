Guide to Searching by Tags
==========================

## Overview ##

A search using tags takes the form of an expression in the search selection
language, the form and usage of which is described in this document.

### Expressions ###

The tag selection language is composed of expressions. An expression consists
of a series of predicates forms and selection operators applied to those forms.
In place of a predicate form, one can also use a nested expression. Nested
expressions begin with the `{` character and end with the `}` character.

## Identifiers ##

The most basic unit of an expression is an identifier. Identifers are strings of
text that begin and end with a non-whitespace character, but can contain
whitespace inside of them. Identifers cannot contain a reserved character,
unless that character is escaped. Otherwise, the presence of a reserved
character signified a boundary of a tag.

### Reserved Characters ###

The set of reserved characters, displayed with each character separated by a
space, is as follows: `: ~ , ^ | ? = ; { }`

### White Space ###

The term "whitespace" refers to invisible characters such as spaces, tabs,
newlines, and carriage returns.  

Whitespace characters that are inside of identifiers are treated as part of
them, and are interpreted as is. For a predicate or argument with the identifier
`a name`, the space between the words `a` and `name` is part of the identifier
itself. Thus the identifier `a name` is different and distinct from the
identifier `a   name`.

Whitespace between a non-escaped reserved character and a non-reserved
non-whitespace character is not considered part of an identifer, so the
the string `  x ;  predicate  : an argument ; y ` is interpreted as a sequence
of the tokens `x`, `;`, `predicate`, `:`, `an argument`, `;`, and `y` as the
whitespace surrounding reserved characters is not significant.

### Escaping Characters ###

In order to inlcude a reserved character in a predicate argument, that character
must be preceeded by the escape character `\` as in the following
`tag with\, comma`, in this case, the comma is part of the tag name and it is
interpreted as being `tag with, comma`. Non-reserved characters preceeded by
the escape character are treated as is, so `a\p\p\le` is treated the same as
`apple`. In order to include a literal `\` in a tag name, it must also be
preceeded by an escape character as in `literal \\\\ character` which is read
as `literal \\ character`

## Predicate Forms ##

Predicates forms are composed of two identifers in the form:
`[Predicate] [:] [Argument]`

They are the smallest semantic unit of a search, i.e. the smallest unit of a
search that can actually be executed on to produce results.

The predicate section preceeding the colon specifies a test to perform on each
item to be filtered, and the argument specifies a value to test for. Each
predicate form produces a boolean value that can be transfromed and combined
using operators.

For example, the predicate form `score: neutral` invokes the predicate
`score` with the argument `neutral` and returns as theresult of its search
all of the items that have a score of `neutral`. Similarly, `score: high`
returns all of the items that have a score of `high`, etc...

### Tags ###

Tags are little descriptive strings of text, in the form of identifers, that are
attached to items to describe a quality of that item. The predicate form for
searching for items based on tags has special treatment, in that it has an
implicit predicate, i.e. a predicate form consisting of a lone, bare identifer
with no `:` is implicitly treated as the argument to the `tag` predicate.

However the `tag` predicate can also be invoked explicitly, and as such the
expression `cats, dogs` is equivalent to the expression `tag: cats, tag: dogs`

## Operators ##

Operators take the results of applying predicates and combine and transform them
using boolean logic.

### Operands ###

Each operator takes one or two operands, and operand consists of either a
predicate, or an expression. Each operand is received by the operator as the
boolean value produced by evaluation that predicate or expression.

Which expressions are taken as operands to which operators is determined by
the precedence of the operators they contain. Expressions using operators
with a higher precedence, signified by a lower number, will be evalued before
and used as operands for those with a lower precedence, signified by a higher
value. Those inside of curly braces `{}` are considered to have an effective
precedence of 0. 

The `:` character is not an operator, and predicate and argument of a predicate
form cannot be nested in the way that operands to operators can be.

### Precedence ###

In the absence of braces the operators are evaluated in the following order,
starting from the lowest number and proceeding to the highest:

1. `~`
2. `,`
3. `^`
4. `|`
5. `?`
6. `=`
7. `;`

### Unary Operators ###

There is only one unary operator, the logical not operator. It takes only one
operand to its right, in the form `[operator] [operand]`

#### The Not Operator ####

The Not operator `~` selects the items that are not selected by the expression
on its right hand side. Items that are selected by the expression on its right
hand side are filtered out.

### Binary Operators ###

There are five binary operators. They each take two operands, one to the left
and one to the right, in the form `[operand A] [operator] [operand B]`

Binary operators are either commutative or non-commutative. For a commutative
operator, the expression  `[A] [operator] [B]` is eqivalent to the
expression `[B] [operator] [A]` but the same is not true for a
non-commutative operator.

#### The And Operator ####

The And operator `,` selects items that are selected by both the expression on
its right hand side as well as the expression on its left hand side. Items only
selected by one expression or by neither are filtered out.

The And operator is commutative.

### The Exclusive Or Operator ####

The Exclusive Or operator `^` selects items that are selected by either the
expression on its right hand side or the expression on its left hand side, but
not by both. Items selected by both expression and items selected by neither
expression are filtered out.

The Exclusive Or operator is commutative.

#### The Inclusive Or Operator ####

The Inclusive Or operator `|` selects items that are selected by the expression
on its right hand side, by the expression on its left hand side, or by both of
these expressions. Items not selected by either expression are filtered out.

The Inclusive Or operator is commutative.

#### The Only If Operator ####

The Only If operator `?` works slightly differently from the other binary
operators. It selects all items not selected by the expression on its left hand
side, and for the items that are selected by the expression on its left hand
side, it selects only those items that are also selected by its right hand side.
It filters out items that are selected by its left hand side by not its right
hand side.

The Only If operator is non-commutative.

#### The Equivalence Operator ####

The Equivalence Operator `=` selects items that are either selected by both
the expression on its right hand side and its left hand side, or by neither of
these expressions. Items only selected by one side are filtered out.

The Equivalence operator is commutative.

#### The Statement Operator ####

The Statement Operator `;` selects the items selected by the expression on its 
right hand side, ignoring the result of the expression on its left hand side.
This is useful for use with predicates that have side-effects, as a way to apply
them specifically for those side-effects.

The Statement Operator is non-commutative.

## Examples ##

The expression `dogs, {beach? crabs ^ seagulls}` selects every picture with
a dog, and for those on a beach, filters out those without any crabs or seagulls
and those that have both crabs and seagulls.

The expression `fantasy = animation | novel` selects every story that is in
the fantasy genre that is either an animation or a novel or both and every
non-fantasy work that is neither an animation nor a novel.
