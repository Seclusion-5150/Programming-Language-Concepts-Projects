package plc.project;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Stream;

/**
 * Standard JUnit5 parameterized tests. See the RegexTests file from Homework 1
 * or the LexerTests file from the last project part for more information.
 */
final class ParserExpressionTests {

    @ParameterizedTest
    @MethodSource
    void testExpressionStatement(String test, List<Token> tokens, Ast.Statement.Expression expected) {
        test(tokens, expected, Parser::parseStatement);
    }

    private static Stream<Arguments> testExpressionStatement() {
        return Stream.of(
                Arguments.of("Function Expression",
                        Arrays.asList(
                                // name();
                                new Token(Token.Type.IDENTIFIER, "name", 0),
                                new Token(Token.Type.OPERATOR, "(", 4),
                                new Token(Token.Type.OPERATOR, ")", 5),
                                new Token(Token.Type.OPERATOR, ";", 6)
                        ),
                        new Ast.Statement.Expression(new Ast.Expression.Function(Optional.empty(), "name", Arrays.asList()))
                )
        );
    }

    @ParameterizedTest
    @MethodSource
    void testAssignmentStatement(String test, List<Token> tokens, Ast.Statement.Assignment expected) {
        test(tokens, expected, Parser::parseStatement);
    }

    private static Stream<Arguments> testAssignmentStatement() {
        return Stream.of(
                Arguments.of("Assignment",
                        Arrays.asList(
                                // name = value;
                                new Token(Token.Type.IDENTIFIER, "name", 0),
                                new Token(Token.Type.OPERATOR, "=", 5),
                                new Token(Token.Type.IDENTIFIER, "value", 7),
                                new Token(Token.Type.OPERATOR, ";", 12)
                        ),
                        new Ast.Statement.Assignment(
                                new Ast.Expression.Access(Optional.empty(), "name"),
                                new Ast.Expression.Access(Optional.empty(), "value")
                        )
                )
        );
    }

    @ParameterizedTest
    @MethodSource
    void testLiteralExpression(String test, List<Token> tokens, Ast.Expression.Literal expected) {
        test(tokens, expected, Parser::parseExpression);
    }

    private static Stream<Arguments> testLiteralExpression() {
        return Stream.of(
                Arguments.of("Boolean Literal",
                        Arrays.asList(new Token(Token.Type.IDENTIFIER, "TRUE", 0)),
                        new Ast.Expression.Literal(Boolean.TRUE)
                ),
                Arguments.of("Integer Literal",
                        Arrays.asList(new Token(Token.Type.INTEGER, "1", 0)),
                        new Ast.Expression.Literal(new BigInteger("1"))
                ),
                Arguments.of("Decimal Literal",
                        Arrays.asList(new Token(Token.Type.DECIMAL, "2.0", 0)),
                        new Ast.Expression.Literal(new BigDecimal("2.0"))
                ),
                Arguments.of("Character Literal",
                        Arrays.asList(new Token(Token.Type.CHARACTER, "'c'", 0)),
                        new Ast.Expression.Literal('c')
                ),
                Arguments.of("String Literal",
                        Arrays.asList(new Token(Token.Type.STRING, "\"string\"", 0)),
                        new Ast.Expression.Literal("string")
                ),
                Arguments.of("Escape Character",
                        Arrays.asList(new Token(Token.Type.STRING, "\"Hello,\\nWorld!\"", 0)),
                        new Ast.Expression.Literal("Hello,\nWorld!")
                )
        );
    }

    @ParameterizedTest
    @MethodSource
    void testGroupExpression(String test, List<Token> tokens, Ast.Expression.Group expected) {
        test(tokens, expected, Parser::parseExpression);
    }

    private static Stream<Arguments> testGroupExpression() {
        return Stream.of(
                Arguments.of("Grouped Variable",
                        Arrays.asList(
                                // (expr)
                                new Token(Token.Type.OPERATOR, "(", 0),
                                new Token(Token.Type.IDENTIFIER, "expr", 1),
                                new Token(Token.Type.OPERATOR, ")", 5)
                        ),
                        new Ast.Expression.Group(new Ast.Expression.Access(Optional.empty(), "expr"))
                ),
                Arguments.of("Grouped Binary",
                        Arrays.asList(
                                // (expr1 + expr2)
                                new Token(Token.Type.OPERATOR, "(", 0),
                                new Token(Token.Type.IDENTIFIER, "expr1", 1),
                                new Token(Token.Type.OPERATOR, "+", 7),
                                new Token(Token.Type.IDENTIFIER, "expr2", 9),
                                new Token(Token.Type.OPERATOR, ")", 14)
                        ),
                        new Ast.Expression.Group(new Ast.Expression.Binary("+",
                                new Ast.Expression.Access(Optional.empty(), "expr1"),
                                new Ast.Expression.Access(Optional.empty(), "expr2")
                        ))
                )
        );
    }

    @ParameterizedTest
    @MethodSource
    void testBinaryExpression(String test, List<Token> tokens, Ast.Expression.Binary expected) {
        test(tokens, expected, Parser::parseExpression);
    }

    private static Stream<Arguments> testBinaryExpression() {
        return Stream.of(
                Arguments.of("Binary And",
                        Arrays.asList(
                                // expr1 && expr2
                                new Token(Token.Type.IDENTIFIER, "expr1", 0),
                                new Token(Token.Type.OPERATOR, "&&", 6),
                                new Token(Token.Type.IDENTIFIER, "expr2", 10)
                        ),
                        new Ast.Expression.Binary("&&",
                                new Ast.Expression.Access(Optional.empty(), "expr1"),
                                new Ast.Expression.Access(Optional.empty(), "expr2")
                        )
                ),
                Arguments.of("Binary Equality",
                        Arrays.asList(
                                // expr1 == expr2
                                new Token(Token.Type.IDENTIFIER, "expr1", 0),
                                new Token(Token.Type.OPERATOR, "==", 6),
                                new Token(Token.Type.IDENTIFIER, "expr2", 9)
                        ),
                        new Ast.Expression.Binary("==",
                                new Ast.Expression.Access(Optional.empty(), "expr1"),
                                new Ast.Expression.Access(Optional.empty(), "expr2")
                        )
                ),
                Arguments.of("Binary Addition",
                        Arrays.asList(
                                // expr1 + expr2
                                new Token(Token.Type.IDENTIFIER, "expr1", 0),
                                new Token(Token.Type.OPERATOR, "+", 6),
                                new Token(Token.Type.IDENTIFIER, "expr2", 8)
                        ),
                        new Ast.Expression.Binary("+",
                                new Ast.Expression.Access(Optional.empty(), "expr1"),
                                new Ast.Expression.Access(Optional.empty(), "expr2")
                        )
                ),
                Arguments.of("Binary Multiplication",
                        Arrays.asList(
                                // expr1 * expr2
                                new Token(Token.Type.IDENTIFIER, "expr1", 0),
                                new Token(Token.Type.OPERATOR, "*", 6),
                                new Token(Token.Type.IDENTIFIER, "expr2", 8)
                        ),
                        new Ast.Expression.Binary("*",
                                new Ast.Expression.Access(Optional.empty(), "expr1"),
                                new Ast.Expression.Access(Optional.empty(), "expr2")
                        )
                )
        );
    }

    @ParameterizedTest
    @MethodSource
    void testAccessExpression(String test, List<Token> tokens, Ast.Expression.Access expected) {
        test(tokens, expected, Parser::parseExpression);
    }

    private static Stream<Arguments> testAccessExpression() {
        return Stream.of(
                Arguments.of("Variable",
                        // name
                        Arrays.asList(new Token(Token.Type.IDENTIFIER, "name", 0)),
                        new Ast.Expression.Access(Optional.empty(), "name")
                ),

                Arguments.of("Field Access",
                        Arrays.asList(
                                // obj.field
                                new Token(Token.Type.IDENTIFIER, "obj", 0),
                                new Token(Token.Type.OPERATOR, ".", 3),
                                new Token(Token.Type.IDENTIFIER, "field", 4)
                        ),
                        new Ast.Expression.Access(Optional.of(new
                                Ast.Expression.Access(Optional.empty(), "obj")), "field")
                )
        );
    }

    @ParameterizedTest
    @MethodSource
    void testFunctionExpression(String test, List<Token> tokens, Ast.Expression.Function expected) {
        test(tokens, expected, Parser::parseExpression);
    }

    private static Stream<Arguments> testFunctionExpression() {
        return Stream.of(
                Arguments.of("Zero Arguments",
                        Arrays.asList(
                                // name()
                                new Token(Token.Type.IDENTIFIER, "name", 0),
                                new Token(Token.Type.OPERATOR, "(", 4),
                                new Token(Token.Type.OPERATOR, ")", 5)
                        ),
                        new Ast.Expression.Function(Optional.empty(), "name", Arrays.asList())
                ),
                Arguments.of("Multiple Arguments",
                        Arrays.asList(
                                // name(expr1, expr2, expr3)
                                new Token(Token.Type.IDENTIFIER, "name", 0),
                                new Token(Token.Type.OPERATOR, "(", 4),
                                new Token(Token.Type.IDENTIFIER, "expr1", 5),
                                new Token(Token.Type.OPERATOR, ",", 10),
                                new Token(Token.Type.IDENTIFIER, "expr2", 12),
                                new Token(Token.Type.OPERATOR, ",", 17),
                                new Token(Token.Type.IDENTIFIER, "expr3", 19),
                                new Token(Token.Type.OPERATOR, ")", 24)
                        ),
                        new Ast.Expression.Function(Optional.empty(), "name", Arrays.asList(
                                new Ast.Expression.Access(Optional.empty(), "expr1"),
                                new Ast.Expression.Access(Optional.empty(), "expr2"),
                                new Ast.Expression.Access(Optional.empty(), "expr3")
                        ))
                )
        );
    }

    /**
     * Standard test function. If expected is null, a ParseException is expected
     * to be thrown (not used in the provided tests).
     */
    private static <T extends Ast> void test(List<Token> tokens, T expected, Function<Parser, T> function) {
        Parser parser = new Parser(tokens);
        if (expected != null) {
            Assertions.assertEquals(expected, function.apply(parser));
        } else {
            Assertions.assertThrows(ParseException.class, () -> function.apply(parser));
        }
    }

    @ParameterizedTest
    @MethodSource
    void testOperatorPrecedence(String test, List<Token> tokens, Ast.Expression expected) {
        test(tokens, expected, Parser::parseExpression);
    }

    private static Stream<Arguments> testOperatorPrecedence() {
        return Stream.of(
                Arguments.of("Addition and Multiplication",
                        Arrays.asList(
                                // 1 + 2 * 3
                                new Token(Token.Type.INTEGER, "1", 0),
                                new Token(Token.Type.OPERATOR, "+", 2),
                                new Token(Token.Type.INTEGER, "2", 4),
                                new Token(Token.Type.OPERATOR, "*", 6),
                                new Token(Token.Type.INTEGER, "3", 8)
                        ),
                        new Ast.Expression.Binary("+",
                                new Ast.Expression.Literal(new BigInteger("1")),
                                new Ast.Expression.Binary("*",
                                        new Ast.Expression.Literal(new BigInteger("2")),
                                        new Ast.Expression.Literal(new BigInteger("3"))
                                )
                        )
                ),
                Arguments.of("Comparison and Addition",
                        Arrays.asList(
                                // x < y + z
                                new Token(Token.Type.IDENTIFIER, "x", 0),
                                new Token(Token.Type.OPERATOR, "<", 2),
                                new Token(Token.Type.IDENTIFIER, "y", 4),
                                new Token(Token.Type.OPERATOR, "+", 6),
                                new Token(Token.Type.IDENTIFIER, "z", 8)
                        ),
                        new Ast.Expression.Binary("<",
                                new Ast.Expression.Access(Optional.empty(), "x"),
                                new Ast.Expression.Binary("+",
                                        new Ast.Expression.Access(Optional.empty(), "y"),
                                        new Ast.Expression.Access(Optional.empty(), "z")
                                )
                        )
                )
        );
    }

    @ParameterizedTest
    @MethodSource
    void testLeftAssociativity(String test, List<Token> tokens, Ast.Expression expected) {
        test(tokens, expected, Parser::parseExpression);
    }

    private static Stream<Arguments> testLeftAssociativity() {
        return Stream.of(
                Arguments.of("Addition Left Associative",
                        Arrays.asList(
                                // 1 + 2 + 3
                                new Token(Token.Type.INTEGER, "1", 0),
                                new Token(Token.Type.OPERATOR, "+", 2),
                                new Token(Token.Type.INTEGER, "2", 4),
                                new Token(Token.Type.OPERATOR, "+", 6),
                                new Token(Token.Type.INTEGER, "3", 8)
                        ),
                        new Ast.Expression.Binary("+",
                                new Ast.Expression.Binary("+",
                                        new Ast.Expression.Literal(new BigInteger("1")),
                                        new Ast.Expression.Literal(new BigInteger("2"))
                                ),
                                new Ast.Expression.Literal(new BigInteger("3"))
                        )
                )
        );
    }

    @ParameterizedTest
    @MethodSource
    void testAllComparisonOperators(String test, List<Token> tokens, Ast.Expression expected) {
        test(tokens, expected, Parser::parseExpression);
    }

    private static Stream<Arguments> testAllComparisonOperators() {
        return Stream.of(
                Arguments.of("Less Than",
                        Arrays.asList(
                                new Token(Token.Type.IDENTIFIER, "x", 0),
                                new Token(Token.Type.OPERATOR, "<", 2),
                                new Token(Token.Type.IDENTIFIER, "y", 4)
                        ),
                        new Ast.Expression.Binary("<",
                                new Ast.Expression.Access(Optional.empty(), "x"),
                                new Ast.Expression.Access(Optional.empty(), "y")
                        )
                ),
                Arguments.of("Less Than or Equal",
                        Arrays.asList(
                                new Token(Token.Type.IDENTIFIER, "x", 0),
                                new Token(Token.Type.OPERATOR, "<=", 2),
                                new Token(Token.Type.IDENTIFIER, "y", 5)
                        ),
                        new Ast.Expression.Binary("<=",
                                new Ast.Expression.Access(Optional.empty(), "x"),
                                new Ast.Expression.Access(Optional.empty(), "y")
                        )
                ),
                Arguments.of("Greater Than",
                        Arrays.asList(
                                new Token(Token.Type.IDENTIFIER, "x", 0),
                                new Token(Token.Type.OPERATOR, ">", 2),
                                new Token(Token.Type.IDENTIFIER, "y", 4)
                        ),
                        new Ast.Expression.Binary(">",
                                new Ast.Expression.Access(Optional.empty(), "x"),
                                new Ast.Expression.Access(Optional.empty(), "y")
                        )
                ),
                Arguments.of("Not Equal",
                        Arrays.asList(
                                new Token(Token.Type.IDENTIFIER, "x", 0),
                                new Token(Token.Type.OPERATOR, "!=", 2),
                                new Token(Token.Type.IDENTIFIER, "y", 5)
                        ),
                        new Ast.Expression.Binary("!=",
                                new Ast.Expression.Access(Optional.empty(), "x"),
                                new Ast.Expression.Access(Optional.empty(), "y")
                        )
                )
        );
    }

    @ParameterizedTest
    @MethodSource
    void testLogicalOperators(String test, List<Token> tokens, Ast.Expression expected) {
        test(tokens, expected, Parser::parseExpression);
    }

    private static Stream<Arguments> testLogicalOperators() {
        return Stream.of(
                Arguments.of("Logical AND",
                        Arrays.asList(
                                new Token(Token.Type.IDENTIFIER, "x", 0),
                                new Token(Token.Type.OPERATOR, "AND", 2),
                                new Token(Token.Type.IDENTIFIER, "y", 6)
                        ),
                        new Ast.Expression.Binary("AND",
                                new Ast.Expression.Access(Optional.empty(), "x"),
                                new Ast.Expression.Access(Optional.empty(), "y")
                        )
                ),
                Arguments.of("Logical OR",
                        Arrays.asList(
                                new Token(Token.Type.IDENTIFIER, "x", 0),
                                new Token(Token.Type.OPERATOR, "OR", 2),
                                new Token(Token.Type.IDENTIFIER, "y", 5)
                        ),
                        new Ast.Expression.Binary("OR",
                                new Ast.Expression.Access(Optional.empty(), "x"),
                                new Ast.Expression.Access(Optional.empty(), "y")
                        )
                )
        );
    }

    @ParameterizedTest
    @MethodSource
    void testMissingLiterals(String test, List<Token> tokens, Ast.Expression expected) {
        test(tokens, expected, Parser::parseExpression);
    }

    private static Stream<Arguments> testMissingLiterals() {
        return Stream.of(
                Arguments.of("NIL Literal",
                        Arrays.asList(new Token(Token.Type.IDENTIFIER, "NIL", 0)),
                        new Ast.Expression.Literal(null)
                ),
                Arguments.of("FALSE Literal",
                        Arrays.asList(new Token(Token.Type.IDENTIFIER, "FALSE", 0)),
                        new Ast.Expression.Literal(Boolean.FALSE)
                ),
                Arguments.of("Negative Integer",
                        Arrays.asList(new Token(Token.Type.INTEGER, "-42", 0)),
                        new Ast.Expression.Literal(new BigInteger("-42"))
                ),
                Arguments.of("Negative Decimal",
                        Arrays.asList(new Token(Token.Type.DECIMAL, "-3.14", 0)),
                        new Ast.Expression.Literal(new BigDecimal("-3.14"))
                )
        );
    }

    @ParameterizedTest
    @MethodSource
    void testMethodCallsOnObjects(String test, List<Token> tokens, Ast.Expression expected) {
        test(tokens, expected, Parser::parseExpression);
    }

    private static Stream<Arguments> testMethodCallsOnObjects() {
        return Stream.of(
                Arguments.of("Method Call with Arguments",
                        Arrays.asList(
                                // obj.method(arg1, arg2)
                                new Token(Token.Type.IDENTIFIER, "obj", 0),
                                new Token(Token.Type.OPERATOR, ".", 3),
                                new Token(Token.Type.IDENTIFIER, "method", 4),
                                new Token(Token.Type.OPERATOR, "(", 10),
                                new Token(Token.Type.IDENTIFIER, "arg1", 11),
                                new Token(Token.Type.OPERATOR, ",", 15),
                                new Token(Token.Type.IDENTIFIER, "arg2", 17),
                                new Token(Token.Type.OPERATOR, ")", 21)
                        ),
                        new Ast.Expression.Function(
                                Optional.of(new Ast.Expression.Access(Optional.empty(), "obj")),
                                "method",
                                Arrays.asList(
                                        new Ast.Expression.Access(Optional.empty(), "arg1"),
                                        new Ast.Expression.Access(Optional.empty(), "arg2")
                                )
                        )
                )
        );
    }
    @ParameterizedTest
    @MethodSource
    void testChainedLogicalOperators(String test, List<Token> tokens, Ast.Expression expected) {
        test(tokens, expected, Parser::parseExpression);
    }

    private static Stream<Arguments> testChainedLogicalOperators() {
        return Stream.of(
                Arguments.of("Chained AND",
                        Arrays.asList(
                                // a AND b AND c
                                new Token(Token.Type.IDENTIFIER, "a", 0),
                                new Token(Token.Type.IDENTIFIER, "AND", 2),
                                new Token(Token.Type.IDENTIFIER, "b", 6),
                                new Token(Token.Type.IDENTIFIER, "AND", 8),
                                new Token(Token.Type.IDENTIFIER, "c", 12)
                        ),
                        new Ast.Expression.Binary("AND",
                                new Ast.Expression.Binary("AND",
                                        new Ast.Expression.Access(Optional.empty(), "a"),
                                        new Ast.Expression.Access(Optional.empty(), "b")
                                ),
                                new Ast.Expression.Access(Optional.empty(), "c")
                        )
                ),
                Arguments.of("Chained OR",
                        Arrays.asList(
                                // x OR y OR z
                                new Token(Token.Type.IDENTIFIER, "x", 0),
                                new Token(Token.Type.IDENTIFIER, "OR", 2),
                                new Token(Token.Type.IDENTIFIER, "y", 5),
                                new Token(Token.Type.IDENTIFIER, "OR", 7),
                                new Token(Token.Type.IDENTIFIER, "z", 10)
                        ),
                        new Ast.Expression.Binary("OR",
                                new Ast.Expression.Binary("OR",
                                        new Ast.Expression.Access(Optional.empty(), "x"),
                                        new Ast.Expression.Access(Optional.empty(), "y")
                                ),
                                new Ast.Expression.Access(Optional.empty(), "z")
                        )
                ),
                Arguments.of("Mixed AND OR",
                        Arrays.asList(
                                // a AND b OR c
                                new Token(Token.Type.IDENTIFIER, "a", 0),
                                new Token(Token.Type.IDENTIFIER, "AND", 2),
                                new Token(Token.Type.IDENTIFIER, "b", 6),
                                new Token(Token.Type.IDENTIFIER, "OR", 8),
                                new Token(Token.Type.IDENTIFIER, "c", 11)
                        ),
                        new Ast.Expression.Binary("OR",
                                new Ast.Expression.Binary("AND",
                                        new Ast.Expression.Access(Optional.empty(), "a"),
                                        new Ast.Expression.Access(Optional.empty(), "b")
                                ),
                                new Ast.Expression.Access(Optional.empty(), "c")
                        )
                )
        );
    }

    @ParameterizedTest
    @MethodSource
    void testChainedArithmetic(String test, List<Token> tokens, Ast.Expression expected) {
        test(tokens, expected, Parser::parseExpression);
    }

    private static Stream<Arguments> testChainedArithmetic() {
        return Stream.of(
                Arguments.of("Three Addition Chain",
                        Arrays.asList(
                                // 1 + 2 + 3
                                new Token(Token.Type.INTEGER, "1", 0),
                                new Token(Token.Type.OPERATOR, "+", 2),
                                new Token(Token.Type.INTEGER, "2", 4),
                                new Token(Token.Type.OPERATOR, "+", 6),
                                new Token(Token.Type.INTEGER, "3", 8)
                        ),
                        new Ast.Expression.Binary("+",
                                new Ast.Expression.Binary("+",
                                        new Ast.Expression.Literal(new BigInteger("1")),
                                        new Ast.Expression.Literal(new BigInteger("2"))
                                ),
                                new Ast.Expression.Literal(new BigInteger("3"))
                        )
                ),
                Arguments.of("Three Subtraction Chain",
                        Arrays.asList(
                                // 10 - 3 - 2
                                new Token(Token.Type.INTEGER, "10", 0),
                                new Token(Token.Type.OPERATOR, "-", 3),
                                new Token(Token.Type.INTEGER, "3", 5),
                                new Token(Token.Type.OPERATOR, "-", 7),
                                new Token(Token.Type.INTEGER, "2", 9)
                        ),
                        new Ast.Expression.Binary("-",
                                new Ast.Expression.Binary("-",
                                        new Ast.Expression.Literal(new BigInteger("10")),
                                        new Ast.Expression.Literal(new BigInteger("3"))
                                ),
                                new Ast.Expression.Literal(new BigInteger("2"))
                        )
                ),
                Arguments.of("Four Addition Chain",
                        Arrays.asList(
                                // 1 + 2 + 3 + 4
                                new Token(Token.Type.INTEGER, "1", 0),
                                new Token(Token.Type.OPERATOR, "+", 2),
                                new Token(Token.Type.INTEGER, "2", 4),
                                new Token(Token.Type.OPERATOR, "+", 6),
                                new Token(Token.Type.INTEGER, "3", 8),
                                new Token(Token.Type.OPERATOR, "+", 10),
                                new Token(Token.Type.INTEGER, "4", 12)
                        ),
                        new Ast.Expression.Binary("+",
                                new Ast.Expression.Binary("+",
                                        new Ast.Expression.Binary("+",
                                                new Ast.Expression.Literal(new BigInteger("1")),
                                                new Ast.Expression.Literal(new BigInteger("2"))
                                        ),
                                        new Ast.Expression.Literal(new BigInteger("3"))
                                ),
                                new Ast.Expression.Literal(new BigInteger("4"))
                        )
                )
        );
    }
}
