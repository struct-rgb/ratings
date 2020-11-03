Guide to Searching by Tags
==========================

## Overview ##

A search using tags takes the form of an expression in the tag selection
language, the form and usage of which is described in this document.

### Expressions ###

The tag selection language is composed of expressions. An expression consists
of a series of tag and selection operators applied to those tags. In place of
a tag, one can also use a nested expression. Nested expressions begin with the
**{** character and end with the **}** character.

## Tags ##

Tags are little descriptive strings of text that are attached to items to
describe a quality of that item. Tags begin and end with a non-whitepace
character, but can contain whitespace inside of them. Tags cannot contain
a reserved character, unless that character is escaped. Otherwise, the presence
of a reserved character signifies a boundary of the tag.

### Reserved Characters ###

The set of reserved characters, displayed with each character separated by a
space, is as follows: **~ , ^ | ? =**

### White Space ###

The term "whitespace" refers to invisible characters such as spaces, tabs,
newlines, and carriage returns.  

Whitespace characters that are inside of tag names are treated as part of that
name, and are interpreted as is. For a tag with the identifier **tag name**, the
space between the words **tag** and **name** is part of the name itself. Thus the
tag **tag name** is different and distinct from the tag **tag   name**.

Whitespace characters that are between tag names and operators are ignored. So
given the expression **left | right**, the spaces around **|** are ignored. This
means that following are all equivalent: **left|right** and **left| right** and
**  left   |right   **

### Escaping Characters ###

In order to inlcude a reserved character in a tag, that character must be
preceeded by the escape character **\\** as in the following **tag with\\, comma**,
in this case, the comma is part of the tag name and it is interpreted as being
**tag with, comma**. Non-reserved characters preceeded by the escape character
are treated as is, so **a\p\p\le** is treated the same as **apple**. In order to
include a literal **\\** in a tag name, it must also be preceeded by an escape
character as in **literal \\\\ character** which is read as **literal \\ character**

## Operators ##

### Precedence ###

In the absence of braces the operators are evaluated in the following order,
starting from the lowest number and proceeding to the highest:

	1. ~
	2. ,
	3. ^
	4. |
	5. ?
	6. =

### Unary Operators ###

There is only one unary operator, the logical not operator. It takes only one
operand to its right, in the form **\[operator\] \[operand\]**

#### The Not Operator ####

The Not operator **~** selects the items that are not selected by the expression
on its right hand side. Items that are selected by the expression on its right
hand side are filtered out.

### Binary Operators ###

There are five binary operators. They each take two operands, one to the left
and one to the right, in the form **\[operand A\] \[operator\] \[operand B\]**

Binary operators are either commutative or non-commutative. For a commutative
operator, the expression  **\[A\] \[operator\] \[B\]** is eqivalent to the
expression **\[B\] \[operator\] \[B\]** but the same is not true for a
non-commutative operator. The only non-commutative binary operator in the
tag selection language is **?**, the Only If operator.

#### The And Operator ####

The And operator **,** selects items that are selected by both the expression on
its right hand side as well as the expression on its left hand side. Items only
selected by one expression or by neither are filtered out.

The And operator is commutative.

### The Exclusive Or Operator ####

The Exclusive Or operator **^** selects items that are selected by either the
expression on its right hand side or the expression on its left hand side, but
not by both. Items selected by both expression and items selected by neither
expression are filtered out.

The Exclusive Or operator is commutative.

#### The Inclusive Or Operator ####

The Inclusive Or operator **|** selects items that are selected by the expression
on its right hand side, by the expression on its left hand side, or by both of
these expressions. Items not selected by either expression are filtered out.

The Inclusive Or operator is commutative.

#### The Only If Operator ####

The Only If operator **?** works slightly differently from the other binary
operators. It selects all items not selected by the expression on its left hand
side, and for the items that are selected by the expression on its left hand
side, it selects only those items that are also selected by its right hand side.
It filters out items that are selected by its left hand side by not its right
hand side.

The Only If operator is non-commutative.

#### The Equivalence Operator ####

The Equivalence Operator **=** selects items that are either selected by both the
expression on its right hand side and its left hand side, or by neither of these
expressions. Items only selected by one side are filtered out.

The Equivalence operator is commutative.

## Examples ##

The expression **dogs, \{beach? crabs ^ seagulls\}** selects every picture with
a dog, and for those on a beach, filters out those without any crabs or seagulls
and those that have both crabs and seagulls.

The expression **fantasy = animation | novel** selects every story that is in
the fantasy genre that is either an animation or a novel or both and every
non-fantasy work that is neither an animation nor a novel.
