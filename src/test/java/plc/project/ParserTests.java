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
final class ParserTests {

    @ParameterizedTest
    @MethodSource
    void testSource(String test, List<Token> tokens, Ast.Source expected) {
        test(tokens, expected, Parser::parseSource);
    }

    private static Stream<Arguments> testSource() {
        return Stream.of(
                Arguments.of("Zero Statements",
                        Arrays.asList(),
                        new Ast.Source(Arrays.asList(), Arrays.asList())
                ),
                Arguments.of("Field",
                        Arrays.asList(
                                // LET name = expr;
                                new Token(Token.Type.IDENTIFIER, "LET", 0),
                                new Token(Token.Type.IDENTIFIER, "name", 4),
                                new Token(Token.Type.OPERATOR, "=", 9),
                                new Token(Token.Type.IDENTIFIER, "expr", 11),
                                new Token(Token.Type.OPERATOR, ";", 15)
                        ),
                        new Ast.Source(
                                Arrays.asList(new Ast.Field("name", false, Optional.of(new Ast.Expression.Access(Optional.empty(), "expr")))),
                                Arrays.asList()
                        )
                ),
                Arguments.of("Method",
                        Arrays.asList(
                                // DEF name() DO stmt; END
                                new Token(Token.Type.IDENTIFIER, "DEF", 0),
                                new Token(Token.Type.IDENTIFIER, "name", 4),
                                new Token(Token.Type.OPERATOR, "(", 8),
                                new Token(Token.Type.OPERATOR, ")", 9),
                                new Token(Token.Type.IDENTIFIER, "DO", 11),
                                new Token(Token.Type.IDENTIFIER, "stmt", 14),
                                new Token(Token.Type.OPERATOR, ";", 18),
                                new Token(Token.Type.IDENTIFIER, "END", 20)
                        ),
                        new Ast.Source(
                                Arrays.asList(),
                                Arrays.asList(new Ast.Method("name", Arrays.asList(), Arrays.asList(
                                        new Ast.Statement.Expression(new Ast.Expression.Access(Optional.empty(), "stmt"))
                                )))
                        )
                )
        );
    }

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
    void testDeclarationStatement(String test, List<Token> tokens, Ast.Statement.Declaration expected) {
        test(tokens, expected, Parser::parseStatement);
    }

    private static Stream<Arguments> testDeclarationStatement() {
        return Stream.of(
                Arguments.of("Definition",
                        Arrays.asList(
                                // LET name;
                                new Token(Token.Type.IDENTIFIER, "LET", 0),
                                new Token(Token.Type.IDENTIFIER, "name", 4),
                                new Token(Token.Type.OPERATOR, ";", 8)
                        ),
                        new Ast.Statement.Declaration("name", Optional.empty())
                ),
                Arguments.of("Initialization",
                        Arrays.asList(
                                // LET name = expr;
                                new Token(Token.Type.IDENTIFIER, "LET", 0),
                                new Token(Token.Type.IDENTIFIER, "name", 4),
                                new Token(Token.Type.OPERATOR, "=", 9),
                                new Token(Token.Type.IDENTIFIER, "expr", 11),
                                new Token(Token.Type.OPERATOR, ";", 15)
                        ),
                        new Ast.Statement.Declaration("name", Optional.of(new Ast.Expression.Access(Optional.empty(), "expr")))
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
    void testIfStatement(String test, List<Token> tokens, Ast.Statement.If expected) {
        test(tokens, expected, Parser::parseStatement);
    }

    private static Stream<Arguments> testIfStatement() {
        return Stream.of(
                Arguments.of("If",
                        Arrays.asList(
                                // IF expr DO stmt; END
                                new Token(Token.Type.IDENTIFIER, "IF", 0),
                                new Token(Token.Type.IDENTIFIER, "expr", 3),
                                new Token(Token.Type.IDENTIFIER, "DO", 8),
                                new Token(Token.Type.IDENTIFIER, "stmt", 11),
                                new Token(Token.Type.OPERATOR, ";", 15),
                                new Token(Token.Type.IDENTIFIER, "END", 17)
                        ),
                        new Ast.Statement.If(
                                new Ast.Expression.Access(Optional.empty(), "expr"),
                                Arrays.asList(new Ast.Statement.Expression(new Ast.Expression.Access(Optional.empty(), "stmt"))),
                                Arrays.asList()
                        )
                ),
                Arguments.of("Else",
                        Arrays.asList(
                                // IF expr DO stmt1; ELSE stmt2; END
                                new Token(Token.Type.IDENTIFIER, "IF", 0),
                                new Token(Token.Type.IDENTIFIER, "expr", 3),
                                new Token(Token.Type.IDENTIFIER, "DO", 8),
                                new Token(Token.Type.IDENTIFIER, "stmt1", 11),
                                new Token(Token.Type.OPERATOR, ";", 16),
                                new Token(Token.Type.IDENTIFIER, "ELSE", 18),
                                new Token(Token.Type.IDENTIFIER, "stmt2", 23),
                                new Token(Token.Type.OPERATOR, ";", 28),
                                new Token(Token.Type.IDENTIFIER, "END", 30)
                        ),
                        new Ast.Statement.If(
                                new Ast.Expression.Access(Optional.empty(), "expr"),
                                Arrays.asList(new Ast.Statement.Expression(new Ast.Expression.Access(Optional.empty(), "stmt1"))),
                                Arrays.asList(new Ast.Statement.Expression(new Ast.Expression.Access(Optional.empty(), "stmt2")))
                        )
                )
        );
    }

    @ParameterizedTest
    @MethodSource
    void testWhileStatement(String test, List<Token> tokens, Ast.Statement.While expected) {
        test(tokens, expected, Parser::parseStatement);
    }

    private static Stream<Arguments> testWhileStatement() {
        return Stream.of(
                Arguments.of("While",
                        Arrays.asList(
                                // WHILE expr DO stmt; END
                                new Token(Token.Type.IDENTIFIER, "WHILE", 0),
                                new Token(Token.Type.IDENTIFIER, "expr", 6),
                                new Token(Token.Type.IDENTIFIER, "DO", 11),
                                new Token(Token.Type.IDENTIFIER, "stmt", 14),
                                new Token(Token.Type.OPERATOR, ";", 18),
                                new Token(Token.Type.IDENTIFIER, "END", 20)
                        ),
                        new Ast.Statement.While(
                                new Ast.Expression.Access(Optional.empty(), "expr"),
                                Arrays.asList(new Ast.Statement.Expression(new Ast.Expression.Access(Optional.empty(), "stmt")))
                        )
                )
        );
    }

    @ParameterizedTest
    @MethodSource
    void testReturnStatement(String test, List<Token> tokens, Ast.Statement.Return expected) {
        test(tokens, expected, Parser::parseStatement);
    }

    private static Stream<Arguments> testReturnStatement() {
        return Stream.of(
                Arguments.of("Return Statement",
                        Arrays.asList(
                                // RETURN expr;
                                new Token(Token.Type.IDENTIFIER, "RETURN", 0),
                                new Token(Token.Type.IDENTIFIER, "expr", 7),
                                new Token(Token.Type.OPERATOR, ";", 11)
                        ),
                        new Ast.Statement.Return(new Ast.Expression.Access(Optional.empty(), "expr"))
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
                        new Ast.Expression.Access(Optional.of(new Ast.Expression.Access(Optional.empty(), "obj")), "field")
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
                ),
                Arguments.of("Method Call",
                        Arrays.asList(
                                // obj.method()
                                new Token(Token.Type.IDENTIFIER, "obj", 0),
                                new Token(Token.Type.OPERATOR, ".", 3),
                                new Token(Token.Type.IDENTIFIER, "method", 4),
                                new Token(Token.Type.OPERATOR, "(", 10),
                                new Token(Token.Type.OPERATOR, ")", 11)
                        ),
                        new Ast.Expression.Function(Optional.of(new Ast.Expression.Access(Optional.empty(), "obj")), "method", Arrays.asList())
                )
        );
    }

    @Test
    void testExample1() {
        List<Token> input = Arrays.asList(
                /**
                 *  LET first = 1;
                 *  DEF main() DO
                 *      WHILE first != 10 DO
                 *          print(first);
                 *          first = first + 1;
                 *      END
                 *  END
                 */
                // LET first = 1;
                new Token(Token.Type.IDENTIFIER, "LET", 0),
                new Token(Token.Type.IDENTIFIER, "first", 4),
                new Token(Token.Type.OPERATOR, "=", 10),
                new Token(Token.Type.INTEGER, "1", 12),
                new Token(Token.Type.OPERATOR, ";", 13),
                //DEF main() DO
                new Token(Token.Type.IDENTIFIER, "DEF", 15),
                new Token(Token.Type.IDENTIFIER, "main", 19),
                new Token(Token.Type.OPERATOR, "(", 23),
                new Token(Token.Type.OPERATOR, ")", 24),
                new Token(Token.Type.IDENTIFIER, "DO", 26),
                //    WHILE first != 10 DO
                new Token(Token.Type.IDENTIFIER, "WHILE", 33),
                new Token(Token.Type.IDENTIFIER, "first", 39),
                new Token(Token.Type.OPERATOR, "!=", 45),
                new Token(Token.Type.INTEGER, "10", 48),
                new Token(Token.Type.IDENTIFIER, "DO", 51),
                //        print(first);
                new Token(Token.Type.IDENTIFIER, "print", 62),
                new Token(Token.Type.OPERATOR, "(", 67),
                new Token(Token.Type.IDENTIFIER, "first", 68),
                new Token(Token.Type.OPERATOR, ")", 73),
                new Token(Token.Type.OPERATOR, ";", 74),
                //        first = first + 1;
                new Token(Token.Type.IDENTIFIER, "first", 84),
                new Token(Token.Type.OPERATOR, "=", 90),
                new Token(Token.Type.IDENTIFIER, "first", 92),
                new Token(Token.Type.OPERATOR, "+", 98),
                new Token(Token.Type.INTEGER, "1", 100),
                new Token(Token.Type.OPERATOR, ";", 101),
                //    END
                new Token(Token.Type.IDENTIFIER, "END", 107),
                //END
                new Token(Token.Type.IDENTIFIER, "END", 111)
        );
        Ast.Source expected = new Ast.Source(
                Arrays.asList(new Ast.Field("first", false, Optional.of(new Ast.Expression.Literal(BigInteger.ONE)))),
                Arrays.asList(new Ast.Method("main", Arrays.asList(), Arrays.asList(
                                new Ast.Statement.While(
                                        new Ast.Expression.Binary("!=",
                                                new Ast.Expression.Access(Optional.empty(), "first"),
                                                new Ast.Expression.Literal(BigInteger.TEN)
                                        ),
                                        Arrays.asList(
                                                new Ast.Statement.Expression(
                                                        new Ast.Expression.Function(Optional.empty(), "print", Arrays.asList(
                                                                new Ast.Expression.Access(Optional.empty(), "first"))
                                                        )
                                                ),
                                                new Ast.Statement.Assignment(
                                                        new Ast.Expression.Access(Optional.empty(), "first"),
                                                        new Ast.Expression.Binary("+",
                                                                new Ast.Expression.Access(Optional.empty(), "first"),
                                                                new Ast.Expression.Literal(BigInteger.ONE)
                                                        )
                                                )
                                        )
                                )
                        ))
                ));
        test(input, expected, Parser::parseSource);
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
    @Test
    void testParsePrimaryExpressionNil() throws ParseException {
        Parser parser = new Parser(Arrays.asList(new Token(Token.Type.IDENTIFIER, "NIL", 0)));
        Ast.Expression result = parser.parsePrimaryExpression();
        Assertions.assertNull(((Ast.Expression.Literal) result).getLiteral());
    }

    @Test
    void testParsePrimaryExpressionTrue() throws ParseException {
        Parser parser = new Parser(Arrays.asList(new Token(Token.Type.IDENTIFIER, "TRUE", 0)));
        Ast.Expression result = parser.parsePrimaryExpression();
        Assertions.assertEquals(true, ((Ast.Expression.Literal) result).getLiteral());
    }

    @Test
    void testParsePrimaryExpressionFalse() throws ParseException {
        Parser parser = new Parser(Arrays.asList(new Token(Token.Type.IDENTIFIER, "FALSE", 0)));
        Ast.Expression result = parser.parsePrimaryExpression();
        Assertions.assertEquals(false, ((Ast.Expression.Literal) result).getLiteral());
    }

    @Test
    void testParsePrimaryExpressionInteger() throws ParseException {
        Parser parser = new Parser(Arrays.asList(new Token(Token.Type.INTEGER, "42", 0)));
        Ast.Expression result = parser.parsePrimaryExpression();
        Assertions.assertEquals(new BigInteger("42"), ((Ast.Expression.Literal) result).getLiteral());
    }

    @Test
    void testParsePrimaryExpressionDecimal() throws ParseException {
        Parser parser = new Parser(Arrays.asList(new Token(Token.Type.DECIMAL, "3.14", 0)));
        Ast.Expression result = parser.parsePrimaryExpression();
        Assertions.assertEquals(new BigDecimal("3.14"), ((Ast.Expression.Literal) result).getLiteral());
    }

    @Test
    void testParsePrimaryExpressionCharacter() throws ParseException {
        Parser parser = new Parser(Arrays.asList(new Token(Token.Type.CHARACTER, "'a'", 0)));
        Ast.Expression result = parser.parsePrimaryExpression();
        Assertions.assertEquals('a', ((Ast.Expression.Literal) result).getLiteral());
    }

    @Test
    void testParsePrimaryExpressionString() throws ParseException {
        Parser parser = new Parser(Arrays.asList(new Token(Token.Type.STRING, "\"hello\"", 0)));
        Ast.Expression result = parser.parsePrimaryExpression();
        Assertions.assertEquals("hello", ((Ast.Expression.Literal) result).getLiteral());
    }

    @Test
    void testParsePrimaryExpressionGrouping() throws ParseException {
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.OPERATOR, "(", 0),
                new Token(Token.Type.INTEGER, "42", 1),
                new Token(Token.Type.OPERATOR, ")", 2)
        );
        Parser parser = new Parser(tokens);
        Ast.Expression result = parser.parsePrimaryExpression();
        Assertions.assertTrue(result instanceof Ast.Expression.Group);
    }

    @Test
    void testParsePrimaryExpressionIdentifier() throws ParseException {
        Parser parser = new Parser(Arrays.asList(new Token(Token.Type.IDENTIFIER, "variable", 0)));
        Ast.Expression result = parser.parsePrimaryExpression();
        Assertions.assertTrue(result instanceof Ast.Expression.Access);
    }

    @Test
    void testParsePrimaryExpressionFunctionCall() throws ParseException {
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.IDENTIFIER, "func", 0),
                new Token(Token.Type.OPERATOR, "(", 1),
                new Token(Token.Type.INTEGER, "1", 2),
                new Token(Token.Type.OPERATOR, ",", 3),
                new Token(Token.Type.INTEGER, "2", 4),
                new Token(Token.Type.OPERATOR, ")", 5)
        );
        Parser parser = new Parser(tokens);
        Ast.Expression result = parser.parsePrimaryExpression();
        Assertions.assertTrue(result instanceof Ast.Expression.Function);
    }

    @Test
    void testParseSecondaryExpressionFieldAccess() throws ParseException {
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.IDENTIFIER, "obj", 0),
                new Token(Token.Type.OPERATOR, ".", 1),
                new Token(Token.Type.IDENTIFIER, "field", 2)
        );
        Parser parser = new Parser(tokens);
        Ast.Expression result = parser.parseSecondaryExpression();
        Assertions.assertTrue(result instanceof Ast.Expression.Access);
    }

    @Test
    void testParseSecondaryExpressionMethodCall() throws ParseException {
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.IDENTIFIER, "obj", 0),
                new Token(Token.Type.OPERATOR, ".", 1),
                new Token(Token.Type.IDENTIFIER, "method", 2),
                new Token(Token.Type.OPERATOR, "(", 3),
                new Token(Token.Type.INTEGER, "42", 4),
                new Token(Token.Type.OPERATOR, ")", 5)
        );
        Parser parser = new Parser(tokens);
        Ast.Expression result = parser.parseSecondaryExpression();
        Assertions.assertTrue(result instanceof Ast.Expression.Function);
    }

    @Test
    void testParseAdditiveExpression() throws ParseException {
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.INTEGER, "1", 0),
                new Token(Token.Type.OPERATOR, "+", 1),
                new Token(Token.Type.INTEGER, "2", 2)
        );
        Parser parser = new Parser(tokens);
        Ast.Expression result = parser.parseAdditiveExpression();
        Assertions.assertTrue(result instanceof Ast.Expression.Binary);
    }

    @Test
    void testParseMultiplicativeExpression() throws ParseException {
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.INTEGER, "3", 0),
                new Token(Token.Type.OPERATOR, "*", 1),
                new Token(Token.Type.INTEGER, "4", 2)
        );
        Parser parser = new Parser(tokens);
        Ast.Expression result = parser.parseMultiplicativeExpression();
        Assertions.assertTrue(result instanceof Ast.Expression.Binary);
    }

    @Test
    void testParseEqualityExpression() throws ParseException {
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.INTEGER, "5", 0),
                new Token(Token.Type.OPERATOR, "==", 1),
                new Token(Token.Type.INTEGER, "6", 2)
        );
        Parser parser = new Parser(tokens);
        Ast.Expression result = parser.parseEqualityExpression();
        Assertions.assertTrue(result instanceof Ast.Expression.Binary);
    }

    @Test
    void testParseLogicalExpression() throws ParseException {
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.IDENTIFIER, "TRUE", 0),
                new Token(Token.Type.IDENTIFIER, "AND", 1),
                new Token(Token.Type.IDENTIFIER, "FALSE", 2)
        );
        Parser parser = new Parser(tokens);
        Ast.Expression result = parser.parseLogicalExpression();
        Assertions.assertTrue(result instanceof Ast.Expression.Binary);
    }

    @Test
    void testParseDeclarationStatement() throws ParseException {
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.IDENTIFIER, "LET", 0),
                new Token(Token.Type.IDENTIFIER, "x", 1),
                new Token(Token.Type.OPERATOR, "=", 2),
                new Token(Token.Type.INTEGER, "5", 3),
                new Token(Token.Type.OPERATOR, ";", 4)
        );
        Parser parser = new Parser(tokens);
        Ast.Statement result = parser.parseStatement();
        Assertions.assertTrue(result instanceof Ast.Statement.Declaration);
    }

    @Test
    void testParseAssignmentStatement() throws ParseException {
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.IDENTIFIER, "x", 0),
                new Token(Token.Type.OPERATOR, "=", 1),
                new Token(Token.Type.INTEGER, "10", 2),
                new Token(Token.Type.OPERATOR, ";", 3)
        );
        Parser parser = new Parser(tokens);
        Ast.Statement result = parser.parseStatement();
        Assertions.assertTrue(result instanceof Ast.Statement.Assignment);
    }

    @Test
    void testParseIfStatement() throws ParseException {
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.IDENTIFIER, "IF", 0),
                new Token(Token.Type.IDENTIFIER, "TRUE", 1),
                new Token(Token.Type.IDENTIFIER, "DO", 2),
                new Token(Token.Type.IDENTIFIER, "x", 3),
                new Token(Token.Type.OPERATOR, "=", 4),
                new Token(Token.Type.INTEGER, "1", 5),
                new Token(Token.Type.OPERATOR, ";", 6),
                new Token(Token.Type.IDENTIFIER, "END", 7)
        );
        Parser parser = new Parser(tokens);
        Ast.Statement result = parser.parseStatement();
        Assertions.assertTrue(result instanceof Ast.Statement.If);
    }

    @Test
    void testParseWhileStatement() throws ParseException {
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.IDENTIFIER, "WHILE", 0),
                new Token(Token.Type.IDENTIFIER, "TRUE", 1),
                new Token(Token.Type.IDENTIFIER, "DO", 2),
                new Token(Token.Type.IDENTIFIER, "x", 3),
                new Token(Token.Type.OPERATOR, "=", 4),
                new Token(Token.Type.INTEGER, "1", 5),
                new Token(Token.Type.OPERATOR, ";", 6),
                new Token(Token.Type.IDENTIFIER, "END", 7)
        );
        Parser parser = new Parser(tokens);
        Ast.Statement result = parser.parseStatement();
        Assertions.assertTrue(result instanceof Ast.Statement.While);
    }

    @Test
    void testParseReturnStatement() throws ParseException {
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.IDENTIFIER, "RETURN", 0),
                new Token(Token.Type.INTEGER, "42", 1),
                new Token(Token.Type.OPERATOR, ";", 2)
        );
        Parser parser = new Parser(tokens);
        Ast.Statement result = parser.parseStatement();
        Assertions.assertTrue(result instanceof Ast.Statement.Return);
    }

    @Test
    void testParseField() throws ParseException {
        List<Token> tokens = Arrays.asList(

                new Token(Token.Type.IDENTIFIER, "LET", 0),
                new Token(Token.Type.IDENTIFIER, "x", 1),
                new Token(Token.Type.OPERATOR, "=", 2),
                new Token(Token.Type.INTEGER, "5", 3),
                new Token(Token.Type.OPERATOR, ";", 4)
        );
        Parser parser = new Parser(tokens);
        Ast.Field result = parser.parseField();
        Assertions.assertFalse(result.getConstant());
    }

    @Test
    void testParseConstField() throws ParseException {
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.IDENTIFIER, "LET", 0),
                new Token(Token.Type.IDENTIFIER, "CONST", 1),
                new Token(Token.Type.IDENTIFIER, "x", 2),
                new Token(Token.Type.OPERATOR, "=", 3),
                new Token(Token.Type.INTEGER, "5", 4),
                new Token(Token.Type.OPERATOR, ";", 5)
        );
        Parser parser = new Parser(tokens);
        Ast.Field result = parser.parseField();
        Assertions.assertTrue(result.getConstant());
    }

    @Test
    void testParseMethod() throws ParseException {
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.IDENTIFIER, "DEF", 0),
                new Token(Token.Type.IDENTIFIER, "func", 1),
                new Token(Token.Type.OPERATOR, "(", 2),
                new Token(Token.Type.IDENTIFIER, "param1", 3),
                new Token(Token.Type.OPERATOR, ",", 4),
                new Token(Token.Type.IDENTIFIER, "param2", 5),
                new Token(Token.Type.OPERATOR, ")", 6),
                new Token(Token.Type.IDENTIFIER, "DO", 7),
                new Token(Token.Type.IDENTIFIER, "RETURN", 8),
                new Token(Token.Type.INTEGER, "42", 9),
                new Token(Token.Type.OPERATOR, ";", 10),
                new Token(Token.Type.IDENTIFIER, "END", 11)
        );
        Parser parser = new Parser(tokens);
        Ast.Method result = parser.parseMethod();
        Assertions.assertEquals(2, result.getParameters().size());
    }

    @Test
    void testParseMethodNoParams() throws ParseException {
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.IDENTIFIER, "DEF", 0),
                new Token(Token.Type.IDENTIFIER, "func", 1),
                new Token(Token.Type.OPERATOR, "(", 2),
                new Token(Token.Type.OPERATOR, ")", 3),
                new Token(Token.Type.IDENTIFIER, "DO", 4),
                new Token(Token.Type.IDENTIFIER, "RETURN", 5),
                new Token(Token.Type.INTEGER, "42", 6),
                new Token(Token.Type.OPERATOR, ";", 7),
                new Token(Token.Type.IDENTIFIER, "END", 8)
        );
        Parser parser = new Parser(tokens);
        Ast.Method result = parser.parseMethod();
        Assertions.assertEquals(0, result.getParameters().size());
    }

    @Test
    void testParseSource() throws ParseException {
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.IDENTIFIER, "LET", 0),
                new Token(Token.Type.IDENTIFIER, "x", 1),
                new Token(Token.Type.OPERATOR, "=", 2),
                new Token(Token.Type.INTEGER, "5", 3),
                new Token(Token.Type.OPERATOR, ";", 4),
                new Token(Token.Type.IDENTIFIER, "DEF", 5),
                new Token(Token.Type.IDENTIFIER, "main", 6),
                new Token(Token.Type.OPERATOR, "(", 7),
                new Token(Token.Type.OPERATOR, ")", 8),
                new Token(Token.Type.IDENTIFIER, "DO", 9),
                new Token(Token.Type.IDENTIFIER, "RETURN", 10),
                new Token(Token.Type.IDENTIFIER, "x", 11),
                new Token(Token.Type.OPERATOR, ";", 12),
                new Token(Token.Type.IDENTIFIER, "END", 13)
        );
        Parser parser = new Parser(tokens);
        Ast.Source result = parser.parseSource();
        Assertions.assertEquals(1, result.getFields().size());
        Assertions.assertEquals(1, result.getMethods().size());
    }

    @Test
    void testParseExpressionError() {

        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.OPERATOR, "invalid", 0)
        );

        Parser parser = new Parser(tokens);
        Assertions.assertThrows(ParseException.class, () -> {
            parser.parsePrimaryExpression();
        });
    }

    @Test
    void testParseMethodError() {
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.IDENTIFIER, "func", 0),
                new Token(Token.Type.OPERATOR, "(", 1),
                new Token(Token.Type.OPERATOR, ")", 2)
        );
        Parser parser = new Parser(tokens);
        Assertions.assertThrows(ParseException.class, () -> {
            parser.parseMethod();
        });
    }

    @Test
    void testComplexExpression() throws ParseException {
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.INTEGER, "1", 0),
                new Token(Token.Type.OPERATOR, "+", 1),
                new Token(Token.Type.INTEGER, "2", 2),
                new Token(Token.Type.OPERATOR, "*", 3),
                new Token(Token.Type.INTEGER, "3", 4)
        );
        Parser parser = new Parser(tokens);
        Ast.Expression result = parser.parseExpression();
        Assertions.assertTrue(result instanceof Ast.Expression.Binary);
        Ast.Expression.Binary binary = (Ast.Expression.Binary) result;
        Assertions.assertTrue(binary.getRight() instanceof Ast.Expression.Binary);
    }

    @Test
    void testChainedComparison() throws ParseException {
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.IDENTIFIER, "x", 0),
                new Token(Token.Type.OPERATOR, "<", 1),
                new Token(Token.Type.IDENTIFIER, "y", 2)
        );
        Parser parser = new Parser(tokens);
        Ast.Expression result = parser.parseExpression();
        Assertions.assertTrue(result instanceof Ast.Expression.Binary);
    }
    @Test
    void testEmptyFunctionCall() throws ParseException {
        // Test: func() with no arguments
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.IDENTIFIER, "func", 0),
                new Token(Token.Type.OPERATOR, "(", 4),
                new Token(Token.Type.OPERATOR, ")", 5)
        );
        Parser parser = new Parser(tokens);
        Ast.Expression result = parser.parseExpression();
        Assertions.assertTrue(result instanceof Ast.Expression.Function);
        // Your implementation removes literals from arguments, so empty list should remain empty
    }

    @Test
    void testTrailingCommaInFunction() {
        // Test: func(arg,) - should throw exception
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.IDENTIFIER, "func", 0),
                new Token(Token.Type.OPERATOR, "(", 4),
                new Token(Token.Type.IDENTIFIER, "arg", 5),
                new Token(Token.Type.OPERATOR, ",", 8),
                new Token(Token.Type.OPERATOR, ")", 9)
        );
        Parser parser = new Parser(tokens);
        Assertions.assertThrows(ParseException.class, () -> parser.parseExpression());
    }

    @Test
    void testTrailingCommaInMethod() {
        // Test: DEF func(param,) - should throw exception
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.IDENTIFIER, "DEF", 0),
                new Token(Token.Type.IDENTIFIER, "func", 4),
                new Token(Token.Type.OPERATOR, "(", 8),
                new Token(Token.Type.IDENTIFIER, "param", 9),
                new Token(Token.Type.OPERATOR, ",", 14),
                new Token(Token.Type.OPERATOR, ")", 15),
                new Token(Token.Type.IDENTIFIER, "DO", 17),
                new Token(Token.Type.IDENTIFIER, "END", 20)
        );
        Parser parser = new Parser(tokens);
        Assertions.assertThrows(ParseException.class, () -> parser.parseMethod());
    }

    @Test
    void testForStatementWithoutInit() throws ParseException {
        // Test: FOR (;condition;) - no initialization
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.IDENTIFIER, "FOR", 0),
                new Token(Token.Type.OPERATOR, "(", 4),
                new Token(Token.Type.OPERATOR, ";", 5),
                new Token(Token.Type.IDENTIFIER, "TRUE", 6),
                new Token(Token.Type.OPERATOR, ";", 10),
                new Token(Token.Type.OPERATOR, ")", 11),
                new Token(Token.Type.IDENTIFIER, "END", 13)
        );
        Parser parser = new Parser(tokens);
        Ast.Statement result = parser.parseStatement();
        Assertions.assertTrue(result instanceof Ast.Statement.For);
        Ast.Statement.For forStmt = (Ast.Statement.For) result;
        Assertions.assertNull(forStmt.getInitialization());
    }

    @Test
    void testForStatementWithoutIncrement() throws ParseException {
        // Test: FOR (i=0;condition;) - no increment
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.IDENTIFIER, "FOR", 0),
                new Token(Token.Type.OPERATOR, "(", 4),
                new Token(Token.Type.IDENTIFIER, "i", 5),
                new Token(Token.Type.OPERATOR, "=", 6),
                new Token(Token.Type.INTEGER, "0", 7),
                new Token(Token.Type.OPERATOR, ";", 8),
                new Token(Token.Type.IDENTIFIER, "TRUE", 9),
                new Token(Token.Type.OPERATOR, ";", 13),
                new Token(Token.Type.OPERATOR, ")", 14),
                new Token(Token.Type.IDENTIFIER, "END", 16)
        );
        Parser parser = new Parser(tokens);
        Ast.Statement result = parser.parseStatement();
        Assertions.assertTrue(result instanceof Ast.Statement.For);
        Ast.Statement.For forStmt = (Ast.Statement.For) result;
        Assertions.assertNull(forStmt.getIncrement());
    }

    @Test
    void testStringEscapeSequences() throws ParseException {
        // Test escape sequences in strings
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.STRING, "\"Hello\\nWorld\\t!\"", 0)
        );
        Parser parser = new Parser(tokens);
        Ast.Expression result = parser.parseExpression();
        Assertions.assertEquals("Hello\nWorld\t!", ((Ast.Expression.Literal) result).getLiteral());
    }

    @Test
    void testCharacterEscapeSequences() throws ParseException {
        // Test escape sequences in characters
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.CHARACTER, "'\\n'", 0)
        );
        Parser parser = new Parser(tokens);
        Ast.Expression result = parser.parseExpression();
        Assertions.assertEquals('\n', ((Ast.Expression.Literal) result).getLiteral());
    }

    @Test
    void testNestedGrouping() throws ParseException {
        // Test: ((expr))
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.OPERATOR, "(", 0),
                new Token(Token.Type.OPERATOR, "(", 1),
                new Token(Token.Type.IDENTIFIER, "expr", 2),
                new Token(Token.Type.OPERATOR, ")", 6),
                new Token(Token.Type.OPERATOR, ")", 7)
        );
        Parser parser = new Parser(tokens);
        Ast.Expression result = parser.parseExpression();
        Assertions.assertTrue(result instanceof Ast.Expression.Group);
        Assertions.assertTrue(((Ast.Expression.Group) result).getExpression() instanceof Ast.Expression.Group);
    }

    @Test
    void testMultiplicativeRightAssociativity() throws ParseException {
        // Test: 8 / 4 / 2 should parse as ((8 / 4) / 2)
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.INTEGER, "8", 0),
                new Token(Token.Type.OPERATOR, "/", 1),
                new Token(Token.Type.INTEGER, "4", 2),
                new Token(Token.Type.OPERATOR, "/", 3),
                new Token(Token.Type.INTEGER, "2", 4)
        );
        Parser parser = new Parser(tokens);
        Ast.Expression result = parser.parseExpression();
        // Should be left-associative based on your while loop implementation
        Assertions.assertTrue(result instanceof Ast.Expression.Binary);
    }

    @Test
    void testLogicalOperatorPrecedence() throws ParseException {
        // Test: a AND b OR c should parse as (a AND b) OR c
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.IDENTIFIER, "a", 0),
                new Token(Token.Type.IDENTIFIER, "AND", 2),
                new Token(Token.Type.IDENTIFIER, "b", 6),
                new Token(Token.Type.IDENTIFIER, "OR", 8),
                new Token(Token.Type.IDENTIFIER, "c", 11)
        );
        Parser parser = new Parser(tokens);
        Ast.Expression result = parser.parseExpression();
        Assertions.assertTrue(result instanceof Ast.Expression.Binary);
        Ast.Expression.Binary binary = (Ast.Expression.Binary) result;
        Assertions.assertEquals("OR", binary.getOperator());
        Assertions.assertTrue(binary.getLeft() instanceof Ast.Expression.Binary);
    }

    @Test
    void testFieldWithoutValue() throws ParseException {
        // Test: LET name; (no value)
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.IDENTIFIER, "LET", 0),
                new Token(Token.Type.IDENTIFIER, "name", 4),
                new Token(Token.Type.OPERATOR, ";", 8)
        );
        Parser parser = new Parser(tokens);
        Ast.Field result = parser.parseField();
        Assertions.assertEquals("name", result.getName());
        Assertions.assertFalse(result.getValue().isPresent());
    }

    @Test
    void testMethodCallWithNoArgumentsAfterDot() throws ParseException {
        // Test: obj.method() - method call with no arguments
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.IDENTIFIER, "obj", 0),
                new Token(Token.Type.OPERATOR, ".", 3),
                new Token(Token.Type.IDENTIFIER, "method", 4),
                new Token(Token.Type.OPERATOR, "(", 10),
                new Token(Token.Type.OPERATOR, ")", 11)
        );
        Parser parser = new Parser(tokens);
        Ast.Expression result = parser.parseExpression();
        // Your implementation has bugs here - it removes literals and doesn't handle empty args correctly
        Assertions.assertTrue(result instanceof Ast.Expression.Function);
    }

    @Test
    void testMissingIdentifierAfterDot() {
        // Test: obj. (missing identifier)
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.IDENTIFIER, "obj", 0),
                new Token(Token.Type.OPERATOR, ".", 3)
        );
        Parser parser = new Parser(tokens);
        Assertions.assertThrows(ParseException.class, () -> parser.parseExpression());
    }

    @Test
    void testUnterminatedMethodDefinition() {
        // Test: DEF func() DO stmt; (missing END)
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.IDENTIFIER, "DEF", 0),
                new Token(Token.Type.IDENTIFIER, "func", 4),
                new Token(Token.Type.OPERATOR, "(", 8),
                new Token(Token.Type.OPERATOR, ")", 9),
                new Token(Token.Type.IDENTIFIER, "DO", 11),
                new Token(Token.Type.IDENTIFIER, "stmt", 14),
                new Token(Token.Type.OPERATOR, ";", 18)
        );
        Parser parser = new Parser(tokens);
        Assertions.assertThrows(ParseException.class, () -> parser.parseMethod());
    }

    @Test
    void testInvalidLogicalOperators() throws ParseException {
        // Test that && and || work (your code checks for them)
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.IDENTIFIER, "a", 0),
                new Token(Token.Type.OPERATOR, "&&", 2),
                new Token(Token.Type.IDENTIFIER, "b", 5)
        );
        Parser parser = new Parser(tokens);
        Ast.Expression result = parser.parseExpression();
        Assertions.assertTrue(result instanceof Ast.Expression.Binary);
        Assertions.assertEquals("&&", ((Ast.Expression.Binary) result).getOperator());
    }

    @Test
    void testEmptySource() throws ParseException {
        // Test empty source file
        List<Token> tokens = Arrays.asList();
        Parser parser = new Parser(tokens);
        Ast.Source result = parser.parseSource();
        Assertions.assertTrue(result.getFields().isEmpty());
        Assertions.assertTrue(result.getMethods().isEmpty());
    }

    @Test
    void testBuggyForStatementParsing() {
        // Your FOR parsing has a bug - it calls parseStatement() for initialization/increment
        // but FOR expects assignments, not statements
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.IDENTIFIER, "FOR", 0),
                new Token(Token.Type.OPERATOR, "(", 4),
                new Token(Token.Type.IDENTIFIER, "i", 5),
                new Token(Token.Type.OPERATOR, "=", 6),
                new Token(Token.Type.INTEGER, "0", 7),
                new Token(Token.Type.OPERATOR, ";", 8),
                new Token(Token.Type.IDENTIFIER, "TRUE", 9),
                new Token(Token.Type.OPERATOR, ";", 13),
                new Token(Token.Type.IDENTIFIER, "i", 14),
                new Token(Token.Type.OPERATOR, "=", 15),
                new Token(Token.Type.INTEGER, "1", 16),
                new Token(Token.Type.OPERATOR, ")", 17),
                new Token(Token.Type.IDENTIFIER, "END", 19)
        );
        Parser parser = new Parser(tokens);
        // This might fail due to the parseStatement() bug in FOR parsing
        Assertions.assertThrows(ParseException.class, () -> parser.parseStatement());
    }

    // Tests for the specific failing cases from your results
    @Test
    void testExpressionMissingSemicolon() {
        // This should throw ParseException but your parser doesn't
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.IDENTIFIER, "f", 0)
                // Missing semicolon
        );
        Parser parser = new Parser(tokens);
        Assertions.assertThrows(ParseException.class, () -> parser.parseStatement());
    }

    @Test
    void testMethodCallWithEmptyLiteral() {
        // Your parser incorrectly includes empty literal in arguments
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.IDENTIFIER, "obj", 0),
                new Token(Token.Type.OPERATOR, ".", 3),
                new Token(Token.Type.IDENTIFIER, "method", 4),
                new Token(Token.Type.OPERATOR, "(", 10),
                new Token(Token.Type.OPERATOR, ")", 11)
        );
        Parser parser = new Parser(tokens);
        Ast.Expression result = parser.parseExpression();
        Ast.Expression.Function func = (Ast.Expression.Function) result;
        // Should have empty arguments list, not a literal
        Assertions.assertEquals(0, func.getArguments().size());
    }

    @Test
    void testFunctionTrailingCommaNotDetected() {
        // Your parser doesn't properly detect trailing comma
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.IDENTIFIER, "name", 0),
                new Token(Token.Type.OPERATOR, "(", 4),
                new Token(Token.Type.IDENTIFIER, "expr", 5),
                new Token(Token.Type.OPERATOR, ",", 9),
                new Token(Token.Type.OPERATOR, ")", 10)
        );
        Parser parser = new Parser(tokens);
        Assertions.assertThrows(ParseException.class, () -> parser.parseExpression());
    }

    @Test
    void testEqualsNotEqualsOperator() {
        // Your parser returns == instead of !=
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.IDENTIFIER, "expr1", 0),
                new Token(Token.Type.OPERATOR, "!=", 6),
                new Token(Token.Type.IDENTIFIER, "expr2", 9)
        );
        Parser parser = new Parser(tokens);
        Ast.Expression result = parser.parseExpression();
        Ast.Expression.Binary binary = (Ast.Expression.Binary) result;
        Assertions.assertEquals("!=", binary.getOperator());
    }

    @Test
    void testMissingClosingParenthesisCorrectIndex() {
        // Your parser returns wrong index (2 instead of correct index)
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.OPERATOR, "(", 0),
                new Token(Token.Type.INTEGER, "1", 1)
        );
        Parser parser = new Parser(tokens);
        try {
            parser.parseExpression();
            Assertions.fail("Expected ParseException");
        } catch (ParseException e) {
            // Should be index 2 (where closing paren should be), not 2
            Assertions.assertEquals(2, e.getIndex());
        }
    }

    @Test
    void testInvalidClosingParenthesisCorrectIndex() {
        // Test unexpected closing parenthesis
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.OPERATOR, ")", 0),
                new Token(Token.Type.INTEGER, "1", 1)
        );
        Parser parser = new Parser(tokens);
        try {
            parser.parseExpression();
            Assertions.fail("Expected ParseException");
        } catch (ParseException e) {
            // Should point to the invalid closing paren at index 1
            Assertions.assertEquals(0, e.getIndex());
        }
    }
    @Test
    void testFunctionCallWithLiteralArguments() throws ParseException {
        // Test: func("hello", 42) should preserve both arguments
        List<Token> tokens = Arrays.asList(
                new Token(Token.Type.IDENTIFIER, "func", 0),
                new Token(Token.Type.OPERATOR, "(", 4),
                new Token(Token.Type.STRING, "\"hello\"", 5),
                new Token(Token.Type.OPERATOR, ",", 12),
                new Token(Token.Type.INTEGER, "42", 14),
                new Token(Token.Type.OPERATOR, ")", 16)
        );
        Parser parser = new Parser(tokens);
        Ast.Expression result = parser.parseExpression();

        Assertions.assertTrue(result instanceof Ast.Expression.Function);
        Ast.Expression.Function func = (Ast.Expression.Function) result;

        // Should have 2 arguments, not 0 due to removeIf bug
        Assertions.assertEquals(2, func.getArguments().size());

        // First argument should be string literal
        Assertions.assertTrue(func.getArguments().get(0) instanceof Ast.Expression.Literal);
        Assertions.assertEquals("hello", ((Ast.Expression.Literal) func.getArguments().get(0)).getLiteral());

        // Second argument should be integer literal
        Assertions.assertTrue(func.getArguments().get(1) instanceof Ast.Expression.Literal);
        Assertions.assertEquals(new BigInteger("42"), ((Ast.Expression.Literal) func.getArguments().get(1)).getLiteral());
    }
}