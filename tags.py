
from sys import argv
from enum import Enum

from typing import Dict, Generic, List, Callable, Optional, Set, Tuple, Any, Type, TypeVar, Union, cast

class ASTType(Enum):
	AND   = ","
	OR    = "|"
	NOT   = "~"
	XOR   = "^"
	MI    = "?"
	EQV   = "="
	BEGIN = "{"
	END   = "}"
	TAG   = "_"
	NONE  = " "
	TRUE  = "T"
	PRED  = ":"
	STAT  = ";"

"""
count: 10 of ""science fiction, ~japanese animation | fantasy""

count: 10 of "count: 20 of 'fantasy, ~isekai'"

count: 10 of #{count: 20 of #{fantasy, ~isekai}}

limit: 10 {science fiction, ~japanese animation | fantasy}

set: x {max score: good, min score: bad}, fantasy | science fiction, get: x | japanese animation ^ get: x

inspect: genre {fantasy, ~isekai}

genre <Rating: Bleach> True

count: Bleach {fantasy, ~isekai}

eval: fantasy\, \~isekai

eval: {fantasy, \~isekai}

eval: #{fantasy, ~isekai}

pragma: sort;

order: recommendation;

class: file type {
	png ^ jpg ^ tiff ^ bmp
}

class: image {
	is: file type,
	is: 
}

"""

class Pair(object):
	"""
	A Pair represents a node of the abstract syntax tree (AST).

	By convention:

	The self.cdr field is used for holding the next item in a list of values,
	such as the argument list of a binary operator, and should be None if the
	node is the last item of a list.

	For a cdr node, (self.type is ASTType.NONE) == True.

	The self.car field is used for holding "value" nodes, and should be None
	unless self.type is ASTType.NONE.
	
	For a car node, (self.type is ASTType.NONE) == False.

	The self.value field and self.position fields should not contain a caller
	provided value unless self.type is ASTType.TAG.
	"""

	def __init__(self,
		asttype  : ASTType,
		car      : Optional["Pair"] = None,
		cdr      : Optional["Pair"] = None,
		value    : str              = "",
		position : Optional[int]    = None,
	):
		self.type     = asttype		
		self.position = position
		self.car      = car
		self.cdr      = cdr

		if asttype is ASTType.TAG or asttype is ASTType.NONE:
			self.value = value
		else:
			self.value = asttype.value

	def takes(self, *cars: Optional["Pair"]) -> "Pair":
		"""
		Creates a linked list from the provided Pairs with self as the head.

		Each argument is made the car node of an ASTType.NONE Pair, each of
		which are joined using their cdr fields to make a singly linked list,
		the self Pair's cdr is then set to the first item of this list.
		"""

		head = self
		for car in cars:
			pair     = Pair(ASTType.NONE, car, None)
			head.cdr = pair
			head     = pair

		return self

	def __str__(self) -> str:
		return "".join(self.__collect_strings([], 0))

	def __repr__(self) -> str:
		return "".join(self.__collect_strings([], 0))

	def __eq__(self, other: object) -> bool:
		if not isinstance(other, Pair):
			return NotImplemented
		return Pair.__walk_eq(self, other)

	def __ne__(self, other: object) -> bool:
		if not isinstance(other, Pair):
			return NotImplemented
		return not Pair.__walk_eq(self, other)

	@staticmethod
	def __walk_eq(a: Optional["Pair"], b: Optional["Pair"]) -> bool:

		# make sure these two are either both None or both lead somewhere
		if (a is None and b is not None) or (a is not None and b is None):
			return False

		# both are None, and we're done
		if a is None and b is None:
			return True

		# they must both not be None in this case
		assert a is not None and b is not None

		# make sure they have the same type
		if a.type != b.type:
			return False

		# make sure they have the same value
		if a.value != b.value:
			return False

		# check the car and cdr for equality
		return Pair.__walk_eq(a.car, b.car) and Pair.__walk_eq(a.cdr, b.cdr)

	def __collect_strings(self, strings: List[str], indent: int) -> List[str]:
		"""
		Recursively traverse the AST, building a list of str for 
		self.__repr__() or self.__str__() to join into a string representation
		of the tree's structure.
		"""

		if self.type != ASTType.NONE:
			strings.append(" " * indent)
			strings.append("{")
			strings.append(self.value)
			# strings.append(f"{self.value} @{self.position}")

		if self.car:
			strings.append("\n")
			self.car.__collect_strings(strings, indent + 2)

		if self.cdr:
			self.cdr.__collect_strings(strings, indent)
		else:
			strings.append("}")

		return strings

class ParserState(object):

	def __init__(self, tokens: List[Tuple[int, str]]):
		self.index  = 0
		self.tokens = tokens
		self.length = len(tokens)
		self.depth  = 0

	def sink(self) -> None:
		""" Increase parser's tracking of nested expression depth by one. """
		self.depth += 1

	def swim(self) -> None:
		""" Decrease parser's tracking of nested expression depth by one. """
		self.depth -= 1

	def is_toplevel(self) -> bool:
		"""
		Return whether the current nested expression depth is at the top level.
		I.e. False if the parser is inside a nested expression, otherwise True.
		"""

		return self.depth == 0

	def to_next(self) -> None:
		""" Move parser to the next token if one exists. """
		if self.index < self.length:
			self.index += 1

	def to_prev(self) -> None:
		""" Move parser to the previous token if one exists. """
		if self.index > 0:
			self.index -= 1

	def next_token(self) -> Optional[str]:
		""" Return the text value of next token if one exists. """
		i = self.index + 1
		if i < self.length:
			return self.tokens[i][1]
		else:
			return None

	def prev_token(self) -> Optional[str]:
		""" Return the text value of previous token if one exists. """
		i = self.index - 1
		if i > 0:
			return self.tokens[i][1]
		else:
			return None

	@property
	def token(self) -> Optional[str]:
		"""
		Return the text value of the current token or None if at then end of
		the token stream.
		"""

		if self.index < self.length:
			return self.tokens[self.index][1]
		else:
			return None

	@property
	def position(self) -> Optional[int]:
		"""
		Return the index of the occurance of the current token in the source
		or None if at the end of the token stream.
		"""

		if self.index < self.length:
			return self.tokens[self.index][0]
		else:
			return None

TERMINALS = {
	ASTType.AND.value,
	ASTType.OR.value,
	ASTType.NOT.value,
	ASTType.XOR.value,
	ASTType.BEGIN.value,
	ASTType.END.value,
	ASTType.MI.value,
	ASTType.EQV.value,
	ASTType.PRED.value,
	ASTType.STAT.value,
}

ILLEGAL_AFTER_VALUE = {
	ASTType.NOT.value,
	ASTType.BEGIN.value,
	ASTType.PRED.value,
}

ESCAPE_CHARACTER = "\\"

QUOTE_CHARACTER  = "$"

# QUOTE_CHARACTERS = {"\"", "\'"}

SPECIAL = TERMINALS | {ESCAPE_CHARACTER}

#######
#        ####   ####    ##   #####  ######
#       #      #    #  #  #  #    # #
#####    ####  #      #    # #    # #####
#            # #      ###### #####  #
#       #    # #    # #    # #      #
#######  ####   ####  #    # #      ######

##########################################

TRANSLATION: Dict[int, str] = {}

def escape(source: str) -> str:
	"""Return the given string with an escape character inserted before each occurance of a reserved character."""

	# create the translation table on the first call
	if not TRANSLATION:

		TRANSLATION[ord(ESCAPE_CHARACTER)] = ESCAPE_CHARACTER * 2

		for terminal in TERMINALS:
			TRANSLATION[ord(terminal)] = ESCAPE_CHARACTER + terminal

	return source.translate(TRANSLATION)

def tagset(source: str, delimiter: str = ","):
	tokens = tokenize(source)
	return {token for position, token in tokens if token not in TERMINALS}

#
#       ###### #    # ###### #####
#       #       #  #  #      #    #
#       #####    ##   #####  #    #
#       #        ##   #      #####
#       #       #  #  #      #   #
####### ###### #    # ###### #    #

###################################

def eat_quote(source: str, position: int = 0) -> Tuple[int, str]:

	start = position

	assert position < len(source) and source[position] == QUOTE_CHARACTER
	position += 1

	if position == len(source) or source[position] != ASTType.BEGIN.value:
		return (position, QUOTE_CHARACTER)
	else:
		position += 1

	depth: int       = 1
	token: List[str] = list()

	while position < len(source):

		character = source[position]

		if   character == ASTType.BEGIN.value:
			depth += 1
		elif character == ASTType.END.value:
			depth -= 1

			if depth == 0:
				position += 1
				return position, "".join(token)
		elif character == ESCAPE_CHARACTER:
			# skip the escape character
			position += 1
			if position >= len(source):
				break
			else:
				character = source[position]
	
		token.append(character)
		position += 1

	raise CompilationError(
		"unclosed quotation",
		position=start,
		source=source,
	)

def eat_escaped_section(source: str, position: int = 0) -> Tuple[int, Optional[str]]:

	if position >= len(source):
		return position, None

	if source[position] == ESCAPE_CHARACTER:
		position += 1
		if position < len(source):
			character = source[position]
			position += 1
			return position, character

	if source[position] == QUOTE_CHARACTER:

		token = list()

		while source[position] == QUOTE_CHARACTER:
			position, quotation = eat_quote(source, position)
			token.append(quotation)

			if position == len(source):
				break

		return position, "".join(token)

	return position, None

def eat_tag(source: str, position: int = 0) -> Tuple[int, int, str]:

	token = list()
	gap   = list()
	start = position

	while True:

		position, quotation = eat_escaped_section(source, position)

		if quotation is not None:
			token.append(quotation)

		# eat a number of non-special characters
		while position < len(source) and source[position] not in SPECIAL and not source[position].isspace():
			token.append(source[position])
			position += 1

		position, quotation = eat_escaped_section(source, position)

		if quotation is not None:
			token.append(quotation)

		# eat a number of whitespace characters
		while position < len(source) and source[position].isspace():
			gap.append(source[position])
			position += 1

		if position == len(source) or source[position] in TERMINALS:
			# we hit a terminal here, return the token
			return start, position, "".join(token)
		else:
			# we hit a non-terminal, add the gap and continue
			token.extend(gap)
			gap.clear()
			continue

def eat_whitespace(source: str, position: int = 0) -> int:

	while position < len(source) and source[position].isspace():
		position += 1

	return position

def tokenize(source: str, position: int = 0) -> List[Tuple[int, str]]:

	tokens   = list()

	position = eat_whitespace(source)
	start    = position

	while position < len(source):
		
		char = source[position]

		if   char in TERMINALS:
			tokens.append((start, char))
			position += 1

			position = eat_whitespace(
				source,
				position=position,
			)
			start = position
		else:
			start, position, token = eat_tag(
				source,
				position=position,
			)
			tokens.append((start, token))
			start = position

	return tokens

def untokenize(tokens : List[Tuple[int, str]]) -> str:
	"""
	Takes a list of tokens and positions as produced by tokenize() and
	returns the original string, less any trailing whitespace.
	"""
	length = 0
	pieces = []
	
	for start, token in tokens:
		if start != 0:
			diff = start - length
			pieces.append(" " * diff)
			length += diff
		pieces.append(token)
		length += len(token)

	return "".join(pieces)

#######
#       #####  #####   ####  #####
#       #    # #    # #    # #    #
#####   #    # #    # #    # #    #
#       #####  #####  #    # #####
#       #   #  #   #  #    # #   #
####### #    # #    #  ####  #    #

###################################

######
#     # ###### ###### # #    # # ##### #  ####  #    #  ####
#     # #      #      # ##   # #   #   # #    # ##   # #
#     # #####  #####  # # #  # #   #   # #    # # #  #  ####
#     # #      #      # #  # # #   #   # #    # #  # #      #
#     # #      #      # #   ## #   #   # #    # #   ## #    #
######  ###### #      # #    # #   #   #  ####  #    #  ####

#############################################################

class CompilationError(Exception):
	"""
	Represents an error in the compilation process due to invalid input.

	A CompilationError is raised when the compiler encounters an error in the 
	user provided source code that renders it incapable of being compiled in
	order to signal the failure of the compilation process.

	A ComplationError is never raised due to an error that occurs as part of
	the compiler itself, nor during the execution of a compiled expression.
	"""

	def __init__(self,
		reason   : str,
		position : Optional[int]  = None,
		token    : Optional[str]  = None,
		ast      : Optional[Pair] = None,
		source   : Optional[str]  = None,
	):
		self.reason   = reason
		self.position = position
		self.token    = token
		self.ast      = ast

		# used for predicates that take escaped code
		self.source   = source 

	def __str__(self) -> str:
		if self.position is not None:
			return f"{self.reason} at {self.position + 1}"
		else:
			return self.reason

	def underline_string(self) -> str:
		""" 
		Produces an underline arrow that points to the character at which the
		compilation error ocurred if printed below the expression.

		Does not take into account width issues due to characters such as
		combining diacritics.
		"""

		if self.position is None:
			return ""
		else:
			return "~" * self.position + "^"

	def __repr__(self) -> str:
		return self.__str__()

######
#     #   ##   #####   ####  ###### #####
#     #  #  #  #    # #      #      #    #
######  #    # #    #  ####  #####  #    #
#       ###### #####       # #      #####
#       #    # #   #  #    # #      #   #
#       #    # #    #  ####  ###### #    #

##########################################

GRAMMAR = \
"""
<program>   ::= <expr>
<expr>      ::= <stat-expr>
<stat-expr> ::= <stat-expr> ";" <eqv-expr> | <eqv-expr> (maybe remove this)
<eqv-expr>  ::= <eqv-expr>  "=" <mi-expr>  | <mi-expr>
<mi-expr>   ::= <mi-expr>   "?" <or-expr>  | <or-expr>
<or-expr>   ::= <or-expr>   "|" <xor-expr> | <xor-expr>
<xor-expr>  ::= <xor-expr>  "^" <and-expr> | <and-expr>
<and-expr>  ::= <and-expr>  "," <not-expr> | <not-expr>
<not-expr>  ::= "~" <value> | <value>
<value>     ::= <sub-expr> | <predicate> | <tag>
<sub-expr>  ::= "{" <expr> "}"
<predicate> ::= <tag> ":" <sub-expr> | <tag> ":" <tag> | <tag> ":" <tag> <sub-expr>
"""

def binary_operator_parser_factory(operator_terminal: ASTType, operand_parser: Callable[[ParserState], Optional[Pair]]) -> Callable[[ParserState], Optional[Pair]]:
	"""
	Returns a function that parses a binary operator expression.

	The operator_terminal is the terminal symbol for the operator.

	The operand_parser is a parser function which parses the expression type
	that represents the operands to this operator, This expression type will
	consequently be parsed as having a precedence greater than the precedence
	of this operator.

	<operator-expr> ::= <operator-expr> <operator-terminal> <operand-expr> | <operator-expr>
	"""

	def parser(state: ParserState) -> Optional[Pair]:
		
		if (car_a := operand_parser(state)) is None:
			return car_a

		while state.token:

			if (operator := parse_terminal(state, operator_terminal)) is not None:
				if state.token: state.to_next()
			else:
				if  state.token in ILLEGAL_AFTER_VALUE:
					raise CompilationError(
						f"expected binary operator but received '{state.token}'",
						position=state.position,
					)
				elif state.is_toplevel() and state.token == ASTType.END.value:
					raise CompilationError(
						f"encountered end of nested expression at top level",
						position=state.position,
					)
				else:
					return car_a

			if (car_b := operand_parser(state)) is not None:
				operator.takes(car_a, car_b)
				car_a = operator
			else:
				return car_b

		return car_a

	return parser

def parse_terminal(state: ParserState, asttype: ASTType) -> Optional[Pair]:
	""" Parses the occurance of a terminal asttype if that terminal occurs """

	if state.token != asttype.value:
		return None

	return Pair(asttype, None, None, position=state.position)

def parse_value_nesting(state: ParserState) -> Optional[Pair]:
	"""
	Parses the nested expression branch of a value.

	"{" <eqv-expr> "}"
	"""

	beginning = state.position
	
	if  state.token != ASTType.BEGIN.value:
		return None
	elif state.token:
		state.to_next()
	
	state.sink()
	pair = parse_stat_expression(state)
	if pair is None: return pair
	state.swim()

	if  state.token != ASTType.END.value:
		raise CompilationError(
			f"unclosed nested expression",
			position=beginning,
		)
	elif state.token:
		state.to_next()
	
	return pair

def parse_value_tag(state: ParserState) -> Pair:
	"""
	Parses the implicit predicate branch of a value.

	<tag>
	"""

	if state.token is None:
		raise CompilationError(
			f"expected tag but reached end of expression",
		)

	if state.token in TERMINALS:
		raise CompilationError(
			f"expected tag but received '{state.token}'",
			position=state.position,
		)

	pair = Pair(ASTType.TAG, None, None,
		value=state.token,
		position=state.position,
	)

	state.to_next()
	return pair

def parse_predicate(state: ParserState) -> Optional[Pair]:
	"""
	Parses the explicit predicate branch of a value

	<tag> ":" <tag>
	"""

	# these two are fatal because they would also fail to parse a tag
	if state.token is None:
		raise CompilationError(
			f"expected tag or predicate but reached end of expression"
		)

	if state.token in TERMINALS:
		raise CompilationError(
			f"expected tag or predicate but received '{state.token}'",
			position=state.position,
		)

	predicate = state.token
	pred_pos  = state.position
	state.to_next()

	# this is not fatal because it may mean the previous item was a tag
	if state.token != ASTType.PRED.value:
		return None
	else:
		term_pos = state.position
		state.to_next()

	# these two are fatal because now this MUST be a predicate
	if state.token is None:
		raise CompilationError(
			f"expected subject for predicate '{predicate}' but reached end of expression"
		)

	if state.token in TERMINALS:
		raise CompilationError(
			f"expected subject for predicate '{predicate}' but received '{state.token}'",
			position=state.position,
		)

	subject = state.token
	sub_pos = state.position
	state.to_next()

	return Pair(ASTType.PRED, position=term_pos).takes(
			Pair(ASTType.TAG, value=predicate, position=pred_pos),
			Pair(ASTType.TAG, value=subject  , position=sub_pos),
		)

def parse_value(state: ParserState) -> Optional[Pair]:
	"""
	Parses a value expression.

	<value> ::= "{" <eqv-expr> "}" | <tag> ":"" <tag> | <tag>
	"""

	save = state.index

	state.index = save
	out = parse_value_nesting(state)
	if out is not None: return out

	state.index = save
	out = parse_predicate(state)
	if out is not None: return out

	state.index = save
	out = parse_value_tag(state)
	return out

def parse_not_case(state: ParserState) -> Optional[Pair]:
	"""
	Parses the negation case of a not expression

	"~" <value>
	"""

	if  state.token != ASTType.NOT.value:
		return None
	elif state.token:
		state.to_next()

	pair = parse_not_expression(state)
	if pair is None: return pair

	return Pair(ASTType.NOT).takes(pair)

def parse_not_expression(state: ParserState) -> Optional[Pair]:
	"""
	Parses a not expression

	<not-expr> ::= "~" <value> | <value>
	"""
	save = state.index

	state.index = save
	out = parse_not_case(state)
	if out is not None: return out

	state.index = save
	out = parse_value(state)
	return out

parse_and_expression = binary_operator_parser_factory(
	ASTType.AND,
	parse_not_expression,
)

parse_xor_expression = binary_operator_parser_factory(
	ASTType.XOR,
	parse_and_expression,
)

parse_or_expression = binary_operator_parser_factory(
	ASTType.OR,
	parse_xor_expression,
)

parse_mi_expression = binary_operator_parser_factory(
	ASTType.MI,
	parse_or_expression,
)

parse_eqv_expression = binary_operator_parser_factory(
	ASTType.EQV,
	parse_mi_expression,
)

parse_stat_expression = binary_operator_parser_factory(
	ASTType.STAT,
	parse_eqv_expression,
)

def parse(tokens: List[Tuple[int, str]]) -> Optional[Pair]:
	state = ParserState(tokens)
	return parse_stat_expression(state)

 #####
#     #  ####  #####  ######  ####  ###### #    #
#       #    # #    # #      #    # #      ##   #
#       #    # #    # #####  #      #####  # #  #
#       #    # #    # #      #  ### #      #  # #
#     # #    # #    # #      #    # #      #   ##
 #####   ####  #####  ######  ####  ###### #    #

 ################################################

BINARY_OPERATORS = {
	ASTType.OR,
	ASTType.XOR,
	ASTType.AND,
	ASTType.MI,
	ASTType.EQV,
	ASTType.STAT,
}

class Predicate(object):

	# key for default predicate in symbol table
	DEFAULT = ""

	def __init__(self,
		name   : str,
		action : Callable[[Any, Any], bool],
		readme : str                         = "",
		parser : Callable[[str], Any]        = str,
		pure   : bool                        = True,
	):
		self.action = action
		self.parser = parser
		self.pure   = pure
		self.name   = name
		self.readme = readme

	def __call__(self, subject: Any, userdata: Any) -> bool:
		return self.action(subject, userdata)

class PredicateDefinitions(Dict[str, Predicate]):

	def __init__(self, action: Callable[[Any, Any], bool], parser: Callable[[str], Any] = str, pure: bool = True):
		super()

		# initialize default predicate
		self.default(action=action, parser=parser, pure=pure)

	def define(self, name, action: Callable[[Any, Any], bool], readme: str = "", parser: Callable[[str], Any] = str, pure: bool = True):

		self[name] = Predicate(name, action=action, parser=parser, pure=pure)

		return self

	def default(self, action: Callable[[Any, Any], bool], parser: Callable[[str], Any] = str, pure: bool = True):

		self.define(Predicate.DEFAULT, action=action, parser=parser, pure=pure)

		return self

	def alias(self, original, duplicate):

		self[duplicate] = self[original]

		return self

PredicateInstance = Tuple[Callable[[Any, Any], bool], Any]
CodeList          = List[Union[ASTType, PredicateInstance]]

def codegen(pair: Optional[Pair], symbols: Dict[str, Predicate]) -> CodeList:

	if "" not in symbols:
		raise ValueError(
			"Predicate symbol table dict must contain a default predicate value with key == Predicate.DEFAULT"
		)

	code = codegen_walk(pair, symbols, [])

	# if codegen generated nothing make a function that's always true
	if len(code) == 0:
		code.append(ASTType.TRUE)

	return code

def codegen_walk(pair: Optional[Pair], symbols: Dict[str, Predicate], code: CodeList) -> CodeList:

	if pair is None: return code

	assert pair is not None

	if pair.type in BINARY_OPERATORS:
		oper = pair.type
		pair = cast(Pair, pair.cdr)
		codegen_walk(pair.car, symbols, code=code)
		pair = cast(Pair, pair.cdr)
		codegen_walk(pair.car, symbols, code=code)
		code.append(oper)
		return code

	elif pair.type == ASTType.NOT:
		pair = cast(Pair, pair.cdr)
		codegen_walk(pair.car, symbols, code=code)
		code.append(ASTType.NOT)
		return code

	elif pair.type == ASTType.TAG:

		predicate = symbols[Predicate.DEFAULT]

		try:
			subject = predicate.parser(pair.value)
		except ValueError as e:
			raise CompilationError(
				f"subject '{pair.value}' could not be parsed for default predicate, {str(e)}",
				position=pair.position,
			)

		code.append((predicate.action, subject))
		return code

	elif pair.type == ASTType.PRED:
		pair      = cast(Pair, pair.cdr)
		car       = cast(Pair, pair.car)

		if car.value not in symbols:
			raise CompilationError(
				f"predicate '{car.value}' is not defined",
				position=car.position,
			)

		predicate = symbols[car.value]
		pair      = cast(Pair, pair.cdr)
		car       = cast(Pair, pair.car)

		try:
			subject = predicate.parser(car.value)
		except ValueError as e:
			raise CompilationError(
				f"subject '{car.value}' could not be parsed for predicate '{predicate.name}', {str(e)}",
				position=car.position,
			)
		
		code.append((predicate.action, subject))
		return code

	else:
		raise RuntimeError("Codegen walked the AST improperly at \n%s" % pair)

 #####
#     #  ####  #    # #####  # #      ###### #####
#       #    # ##  ## #    # # #      #      #    #
#       #    # # ## # #    # # #      #####  #    #
#       #    # #    # #####  # #      #      #####
#     # #    # #    # #      # #      #      #   #
 #####   ####  #    # #      # ###### ###### #    #

###################################################

def generate_ast(source: str) -> Optional[Pair]:

	tokens = tokenize(source)
	if not tokens: return None

	return parse(tokens)

def compile(source: str, symbols: Dict[str, Predicate]) -> CodeList:

	ast = generate_ast(source)

	return codegen(ast, symbols)

 #####
#     # #   # #    # #####   ##   #    #
#        # #  ##   #   #    #  #   #  #
 #####    #   # #  #   #   #    #   ##
      #   #   #  # #   #   ######   ##
#     #   #   #   ##   #   #    #  #  #
 #####    #   #    #   #   #    # #    #

########################################

#     #
#     # #  ####  #    # #      #  ####  #    # ##### # #    #  ####
#     # # #    # #    # #      # #    # #    #   #   # ##   # #    #
####### # #      ###### #      # #      ######   #   # # #  # #
#     # # #  ### #    # #      # #  ### #    #   #   # #  # # #  ###
#     # # #    # #    # #      # #    # #    #   #   # #   ## #    #
#     # #  ####  #    # ###### #  ####  #    #   #   # #    #  ####

####################################################################

ILLEGAL_SEQUENCES = """
Illegal combos
BOF, binary, unary, pred, begin, end, tag, EOF

{BOF} {binary, unary, pred, end}

{binary | unary} {binary | pred | end | EOF}

{begin} {binary | pred | EOF}

{end} {unary | begin | pred | tag}

{pred} {binary | unary | pred | begin | end | EOF}

{tag} {unary, begin, tag}
"""

__HIGHLIGHT_BINARY      = {
	ASTType.AND.value , ASTType.OR.value   ,
	ASTType.XOR.value , ASTType.EQV.value  ,
	ASTType.MI.value  , ASTType.STAT.value ,
}

__HIGHLIGHT_GROUPING    = {
	ASTType.BEGIN.value, ASTType.END.value
}

__HIGHLIGHT_OPERATORS   = __HIGHLIGHT_BINARY | {ASTType.NOT.value}

__HIGHLIGHT_NO_BOF      = __HIGHLIGHT_BINARY | {
	ASTType.PRED.value, ASTType.END.value
}

__HIGHLIGHT_NO_OPERATOR = __HIGHLIGHT_BINARY | {
	ASTType.PRED.value, ASTType.END.value, None
}

__HIGHLIGHT_NO_BEGIN    = __HIGHLIGHT_BINARY | {
	ASTType.END.value, ASTType.PRED.value, None
}

__HIGHLIGHT_OK_END      = __HIGHLIGHT_BINARY | {ASTType.END.value, None}

__HIGHLIGHT_NO_PRED     = __HIGHLIGHT_OPERATORS | {
	ASTType.PRED.value, ASTType.BEGIN.value, ASTType.END.value, None
}

__HIGHLIGHT_OK_TAG      = __HIGHLIGHT_BINARY | {
	ASTType.PRED.value, ASTType.END.value, None
}
	
def __illegal_token_pair(a: str, b: str) -> bool:
	if   a is None:
		return b in __HIGHLIGHT_NO_BOF
	elif a in __HIGHLIGHT_OPERATORS:
		return b in __HIGHLIGHT_NO_OPERATOR
	elif a == ASTType.BEGIN.value:
		return b in __HIGHLIGHT_NO_BEGIN
	elif a == ASTType.END.value:
		return b not in __HIGHLIGHT_OK_END
	elif a == ASTType.PRED.value:
		return b in __HIGHLIGHT_NO_PRED
	else:
		return b not in __HIGHLIGHT_OK_TAG

def highlighting(
	source  : str,
	symbols : Optional[Set[str]] = None,
) -> List[Tuple[int, str, str]]:

	highlights  = []

	tokens = tokenize(source)
	if not tokens: return highlights

	state = ParserState(tokens)
	if not state.token:
		return highlights

	highlighted = False

	def highlight(tag):
		highlights.append((state.position, state.token, tag))

	if __illegal_token_pair(None, state.token):
		highlight("error")
		state.to_next()

	while state.token:

		if __illegal_token_pair(state.token, state.next_token()):
			if not highlighted:
				highlight("error")
			state.to_next()
			if state.token is not None:
				highlight("error")
				highlighted = True
				continue
			else:
				break

		if highlighted:
			highlighted = False
			continue

		if   state.token in __HIGHLIGHT_GROUPING:
			highlight("grouping")
		elif state.token in __HIGHLIGHT_OPERATORS:
			highlight("operator")
		elif state.token == ASTType.PRED.value:
			highlight("predicate")
		elif state.next_token() == ASTType.PRED.value:
			if state.prev_token() == ASTType.PRED.value:
				highlight("error")
				state.to_next()
				if state.token is not None:
					highlight("error")
					highlighted = True
					continue
			elif symbols is not None and state.token not in symbols:
				highlight("error")
			else:
				highlight("predicate")

		state.to_next()

	return highlights

#######
#       #    #   ##   #      #    #   ##   ##### #  ####  #    #
#       #    #  #  #  #      #    #  #  #    #   # #    # ##   #
#####   #    # #    # #      #    # #    #   #   # #    # # #  #
#       #    # ###### #      #    # ######   #   # #    # #  # #
#        #  #  #    # #      #    # #    #   #   # #    # #   ##
#######   ##   #    # ######  ####  #    #   #   #  ####  #    #

################################################################

def operator_and(stack: List[bool]) -> None:
	a = stack.pop()
	b = stack.pop()
	stack.append(a and b)

def operator_or(stack: List[bool]) -> None:
	a = stack.pop()
	b = stack.pop()
	stack.append(a or b)

def operator_xor(stack: List[bool]) -> None:
	a = stack.pop()
	b = stack.pop()
	stack.append((not a and b) or (a and not b))

def operator_eqv(stack: List[bool]) -> None:
	a = stack.pop()
	b = stack.pop()
	stack.append((a and b) or (not a and not b))

def operator_mi(stack: List[bool]) -> None:
	a = stack.pop()
	b = stack.pop()
	stack.append(not b or a)

def operator_true(stack: List[bool]) -> None:
	stack.append(True)

def operator_not(stack: List[bool]) -> None:
	stack.append(not stack.pop())

def operator_statement(stack: List[bool]) -> None:
	a = stack.pop()
	b = stack.pop()
	stack.append(a)

class Filter(object):

	DISPATCH_TABLE = {
		ASTType.AND  : operator_and,
		ASTType.OR   : operator_or,
		ASTType.XOR  : operator_xor,
		ASTType.NOT  : operator_not,
		ASTType.MI   : operator_mi,
		ASTType.EQV  : operator_eqv,
		ASTType.TRUE : operator_true,
		ASTType.STAT : operator_statement,
	}

	def __init__(self, source: Union[CodeList, str], symbols: Dict[str, Predicate]):

		if   isinstance(source, str):
			self.code = compile(cast(str, source), symbols)
		elif isinstance(source, list):
			self.code = cast(CodeList, source)
		else:
			raise TypeError("Invalid source type %s" % type(source))

		self.stack: List[bool] = []

	def __call__(self, userdata: Any) -> bool:

		for operation in self.code:
			if   isinstance(operation, ASTType):
				
				Filter.DISPATCH_TABLE[cast(ASTType, operation)](self.stack)

			elif isinstance(operation, tuple):

				predicate, subject = cast(PredicateInstance, operation)
				self.stack.append(predicate(subject, userdata))

			else:
				raise TypeError("Improperly formed CodeList contains element of type %s" % type(operation))

		return self.stack.pop()

######
#     # #####  ###### #####  #  ####    ##   ##### ######
#     # #    # #      #    # # #    #  #  #    #   #
######  #    # #####  #    # # #      #    #   #   #####
#       #####  #      #    # # #      ######   #   #
#       #   #  #      #    # # #    # #    #   #   #
#       #    # ###### #####  #  ####  #    #   #   ######

#########################################################

 #####
#     # #    # #####  #####   ####  #####  #####
#       #    # #    # #    # #    # #    #   #
 #####  #    # #    # #    # #    # #    #   #
      # #    # #####  #####  #    # #####    #
#     # #    # #      #      #    # #   #    #
 #####   ####  #      #       ####  #    #   #

################################################

DEFAULT_TRUTHY_STRINGS = {
	"true", "t", "y", "yes", "1"
}

DEFAULT_FALSEY_STRINGS = {
	"false", "f", "n", "no", "0"
}

def bool_parser_factory(
	truthy: Set[str] = DEFAULT_TRUTHY_STRINGS,
	falsey: Set[str] = DEFAULT_FALSEY_STRINGS,
) -> Callable[[str], bool]:

	error_string = (
		  "value must one of ("
		+ " ".join(truthy)
		+ ") for true or ("
		+ " ".join(falsey)
		+ ") for false"
	)

	def bool_parser(string: str) -> bool:

		test = string.lower()

		if   test in truthy:
			return True
		elif test in falsey:
			return False
		else:
			raise ValueError(error_string)

	return bool_parser

DEFAULT_BOOL_PARSER = bool_parser_factory()
		
T = TypeVar('T', bound=Enum)

ESPF_MAP = {ord(" "): ord("_")}

def enum_subject_parser_factory(enum: Type[T]) -> Callable[[str], T]:
	def parser(string: str) -> T:
		# try to parse it as an integer
		integer : Optional[int]

		try:
			integer = int(string)
		except ValueError as e:
			integer = None

		# try to construct an enum from this
		if integer is not None:
			# then it's an integer
			try:
				subject = enum(integer)
			except ValueError as e:
				raise ValueError(
					f"integer value for '{enum.__name__}' must be in range {min(enum)} to {max(enum)}"
				)
		else:
			# then it's a string
			try:
				subject = enum[string.translate(ESPF_MAP).upper()]
			except KeyError as e:
				values  = " ".join([value.name for value in enum])
				raise ValueError(
					f"string value for '{enum.__name__}' must be one of uppercase or lowercase: \n{values}"
				)

		return subject
	return parser

O = TypeVar("O")

class Box(Generic[O]):
	"""
	A container than can be used to maintain state between filter invocations.

	Most predicates that take a Box as a predicate will be impure predicates and
	as such should also take pure=False in their constructor.
	"""

	def __init__(self, obj: O):
		self.value = obj

	@staticmethod
	def parser_factory(parser: Callable[[str], Any]) -> Callable[[str], "Box[O]"]:
		"""
		A conveniance function for generating a new parser function that
		automatically wraps the return value of a subject parser in a Box.
		"""

		def wrapper(subject: str) -> "Box[O]":
			return Box(parser(subject))

		return wrapper

def main():
	for argument in argv[1:]:
		ast = generate_ast(argument)

		if isinstance(ast, CompilationError):
			print(argument)
			if ast.position is not None:
				print(ast.underline_string())

		print(ast)

"""
Optimization Table

Only expressions containing pure functions can be eliminated.

For not:
If count("not") is odd, reduce to "<not> <value>" else reduce to "<value>"

below, x represents the result of a ast branch computation, and y is ~x

a	b	a , b	a | b	a ^ b	a = b	a ? b   a ; b
0	0	  0  	  0  	  0  	  1  	  1       0
0	1	  0  	  1  	  1  	  0  	  1       1
1	0	  0  	  1  	  1  	  0  	  0       0
1	1	  1  	  1  	  0  	  1  	  1       1
0	x	  0  	  x  	  x  	  y  	  1       x
x	0	  0  	  x  	  x  	  y  	  y       0
1	x	  x  	  1  	  y  	  x  	  x       x
x	1	  x  	  1  	  y  	  x  	  1       1

these can only be applied if x is pure
x	x	  x  	  x  	  0  	  1  	  x       x
x	y 	  0  	  1  	  1  	  0  	  x       y
y	x 	  0  	  1  	  1  	  0  	  y       x

~x -||- y

"""

if __name__ == "__main__":
	main()
