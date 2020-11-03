
import unittest
from tags import ASTType, CompilationError, Pair, generate_ast

class ParserOutputTest(unittest.TestCase):

	def setUp(self):
		self.a = Pair(ASTType.TAG, value="a")
		self.b = Pair(ASTType.TAG, value="b")
		self.c = Pair(ASTType.TAG, value="c")

	def assertBinaryOperator(self, asttype=None):
		self.assertEqual(
			generate_ast("a" + asttype.value + "b"),
			Pair(asttype).takes(self.a, self.b),
		)

	def test_operator_eqv(self):
		self.assertBinaryOperator(ASTType.EQV)

	def test_operator_and(self):
		self.assertBinaryOperator(ASTType.AND)

	def test_operator_or(self):
		self.assertBinaryOperator(ASTType.OR)

	def test_operator_xor(self):
		self.assertBinaryOperator(ASTType.XOR)

	def test_operator_mi(self):
		self.assertBinaryOperator(ASTType.MI)

	def test_operator_pred(self):
		self.assertBinaryOperator(ASTType.PRED)

	def test_operator_not(self):
		self.assertEqual(
			generate_ast(ASTType.NOT.value + "a"),
			Pair(ASTType.NOT).takes(self.a),
		)

class ParserCompilationErrorTest(unittest.TestCase):

	def assertErrorWith(self, source, reason, position):
		try:
			ast = generate_ast(source)
		except CompilationError as e:
			self.assertIsInstance(e, CompilationError)
			self.assertEqual(reason, e.reason)
			self.assertEqual(position, e.position)
			return

		self.assertTrue(False)

	def test_expr_begins_with_operator(self):
		self.assertErrorWith("=", "expected tag or predicate but received '='", 0)

	def test_expr_begins_with_operator_nested(self):
		self.assertErrorWith("{=}", "expected tag or predicate but received '='", 1)

	def test_expr_missing_rhs_toplevel(self):
		self.assertErrorWith("a=", "expected tag or predicate but reached end of expression", None)

	def test_expr_missing_rhs_nested_unclosed(self):
		self.assertErrorWith("{a=", "expected tag or predicate but reached end of expression", None)

	def test_expr_missing_rhs_nested_closed(self):
		self.assertErrorWith("{a=}", "expected tag or predicate but received '}'", 3)

	def test_expr_adjacent_operators(self):
		self.assertErrorWith("a,,", "expected tag or predicate but received ','", 2)

	def test_expr_unary_operator_on_rhs(self):
		self.assertErrorWith("a~", "expected binary operator but received '~'", 1)

	def test_expr_begin_nested_on_rhs(self):
		self.assertErrorWith("a{", "expected binary operator but received '{'", 1)

	def test_expr_end_nesting_toplevel(self):
		self.assertErrorWith("a}", "encountered end of nested expression at top level", 1)

	def test_expr_enclosed_nesting(self):
		self.assertErrorWith("{a", "unclosed nested expression", 0)

	def test_expr_empty_nesting(self):
		self.assertErrorWith("{}", "expected tag or predicate but received '}'", 1)

	def test_missing_subject_operator(self):
		self.assertErrorWith("a:,", "expected subject for predicate 'a' but received ','", 2)

	def test_missing_subject_end(self):
		self.assertErrorWith("{a:", "expected subject for predicate 'a' but reached end of expression", None)

	def test_missing_subject_end_toplevel(self):
		self.assertErrorWith("a:", "expected subject for predicate 'a' but reached end of expression", None)

if __name__ == '__main__':
    unittest.main()