package plc.project;
import java.lang.reflect.Array;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import static java.lang.Boolean.parseBoolean;
/**
 * The parser takes the sequence of tokens emitted by the lexer and turns that
 * into a structured representation of the program, called the Abstract Syntax
 * Tree (AST).
 *
 * The parser has a similar architecture to the lexer, just with {@link Token}s
 * instead of characters. As before, {@link #peek(Object...)} and {@link
 * #match(Object...)} are helpers to make the implementation easier.
 *
 * This type of parser is called <em>recursive descent</em>. Each rule in our
 * grammar will have it's own function, and reference to other rules correspond
 * to calling those functions.
 */
public final class Parser {
    private final TokenStream tokens;
    public Parser(List<Token> tokens) {
        this.tokens = new TokenStream(tokens);
    }
    /**
     * Parses the {@code source} rule.
     */
    public Ast.Source parseSource() throws ParseException {
        List<Ast.Field> fields = new ArrayList<Ast.Field>();
        List<Ast.Method> methods = new ArrayList<Ast.Method>();
        while(peek("LET") || peek("DEF"))
        {
            if(peek("LET")) fields.add(parseField());
            else if(peek("DEF")) methods.add(parseMethod());
        }
        return new Ast.Source(fields, methods);
    }
    /**
     * Parses the {@code field} rule. This method should only be called if the
     * next tokens start a field, aka {@code LET}.
     */
    public Ast.Field parseField() throws ParseException {
        if(match("LET")) {
            boolean constant = false;
            if (peek("CONST")) {
                match("CONST");
                constant = true;
            }
            if (peek(Token.Type.IDENTIFIER)) {
                String name = tokens.get(0).getLiteral();
                match(Token.Type.IDENTIFIER);
                if (peek("=")) {
                    match("=");
                    Optional<Ast.Expression> expr = Optional.of(parseExpression());
                    if (peek(";")) {
                        match(";");
                        return new Ast.Field(name, constant, expr);
                    } else if (!peek(";")) {
                        int errorIndex = tokens.has(0) ? tokens.get(0).getIndex() :
                                (tokens.get(-1).getIndex() +
                                        tokens.get(-1).getLiteral().length());
                        throw new ParseException("Expected Semicolon at: " +
                                errorIndex, errorIndex);
                    }
                }
                if (peek(";")) {
                    match(";");
                    return new Ast.Field(name, constant, Optional.empty());
                } else if (!peek(";")) {
                    int errorIndex = tokens.has(0) ? tokens.get(0).getIndex() :
                            (tokens.get(-1).getIndex() +
                                    tokens.get(-1).getLiteral().length());
                    throw new ParseException("Expected Semicolon at: " +
                            errorIndex, errorIndex);
                }
            }
        }
        int errorIndex = tokens.has(0) ? tokens.get(0).getIndex() :
                (tokens.get(-1).getIndex() +
                        tokens.get(-1).getLiteral().length());
        throw new ParseException("Invalid Field at: " + errorIndex, errorIndex);
    }
    /**
     * Parses the {@code method} rule. This method should only be called if the
     * next tokens start a method, aka {@code DEF}.
     */
    public Ast.Method parseMethod() throws ParseException {
        if(match("DEF")) {
            if (peek(Token.Type.IDENTIFIER)) {
                String name = tokens.get(0).getLiteral();
                match(Token.Type.IDENTIFIER);
                if (peek("(")) {
                    match("(");
                    List<String> parameters = new ArrayList<String>();
                    if (peek(Token.Type.IDENTIFIER)) {
                        parameters.add(tokens.get(0).getLiteral());
                        match(Token.Type.IDENTIFIER);
                        while (peek(",")) {
                            match(",");
                            if (peek(Token.Type.IDENTIFIER)) {
                                parameters.add(tokens.get(0).getLiteral());
                                match(Token.Type.IDENTIFIER);
                            } else if (!peek(Token.Type.IDENTIFIER)) {
                                int errorIndex = tokens.has(0) ?
                                        tokens.get(0).getIndex() :
                                        (tokens.get(-1).getIndex() +
                                                tokens.get(-1).getLiteral().length());
                                throw new ParseException("Trailing Comma Error at: " + errorIndex, errorIndex);
                            }
                        }
                    }
                    if (peek(")")) {
                        match(")");
                        if (peek("DO")) {
                            match("DO");
                            List<Ast.Statement> methodStatements = new
                                    ArrayList<Ast.Statement>();
                            while (!peek("END") && tokens.has(0)) {
                                methodStatements.add(parseStatement());
                            }
                            if (peek("END")) {
                                match("END");
                                return new Ast.Method(name, parameters,
                                        methodStatements);
                            } else if (!peek("END")) {
                                int errorIndex = tokens.has(0) ?
                                        tokens.get(0).getIndex() :
                                        (tokens.get(-1).getIndex() +
                                                tokens.get(-1).getLiteral().length());
                                throw new ParseException("Expected END Idenfitier at: " + errorIndex, errorIndex);
                            }
                        } else if (!peek("DO")) {
                            int errorIndex = tokens.has(0) ?
                                    tokens.get(0).getIndex() :
                                    (tokens.get(-1).getIndex() +
                                            tokens.get(-1).getLiteral().length());
                            throw new ParseException("Expected DO Idenfitier at: "
                                    + errorIndex, errorIndex);
                        }
                    } else if (!peek(")")) {
                        int errorIndex = tokens.has(0) ? tokens.get(0).getIndex() :
                                (tokens.get(-1).getIndex() +
                                        tokens.get(-1).getLiteral().length());
                        throw new ParseException("Expected Closing Parenthesis at: " + errorIndex, errorIndex);
                    }
                }
            }
        }
        int errorIndex = tokens.has(0) ? tokens.get(0).getIndex() :
                (tokens.get(-1).getIndex() +
                        tokens.get(-1).getLiteral().length());
        throw new ParseException("Invalid Method at: " + errorIndex, errorIndex);
    }
    /**
     * Parses the {@code statement} rule and delegates to the necessary method.
     * If the next tokens do not start a declaration, if, for, while, or return
     * statement, then it is an expression/assignment statement.
     */
    public Ast.Statement parseStatement() throws ParseException {
        // First we have to determine what kind of statement the token is;
        if (peek("LET")) return parseDeclarationStatement();
        else if(peek("IF")) return parseIfStatement();
        else if(peek("FOR")) return parseForStatement();
        else if(peek("WHILE")) return parseWhileStatement();
        else if(peek("RETURN")) return parseReturnStatement();
        else
        {

            Ast.Expression expr = parseExpression();

            if(peek("="))
            {
                match("=");
                Ast.Expression expr2 = parseExpression();
                if(peek(";")) {
                    match(";");
                    return new Ast.Statement.Assignment(expr, expr2);
                }
                else if (!peek(";")) {
                    int errorIndex = tokens.has(0) ? tokens.get(0).getIndex() :
                            (tokens.get(-1).getIndex() +
                                    tokens.get(-1).getLiteral().length());
                    throw new ParseException("Expected Semicolon at: " +
                            errorIndex, errorIndex);
                }
            }
            if(peek(";")) {
                match(";");
                return new Ast.Statement.Expression(expr);
            }
            else if (!peek(";")) {

                int errorIndex = tokens.has(0) ? tokens.get(0).getIndex() :
                        (tokens.get(-1).getIndex() +
                                tokens.get(-1).getLiteral().length());
                throw new ParseException("Expected Semicolon at: " + errorIndex,
                        errorIndex);
            }
        }
        int errorIndex = tokens.has(0) ? tokens.get(0).getIndex() :
                (tokens.get(-1).getIndex() +
                        tokens.get(-1).getLiteral().length());
        throw new ParseException("Invalid Parse Statement at: " + errorIndex,
                errorIndex);
    }
    /**
     * Parses a declaration statement from the {@code statement} rule. This
     * method should only be called if the next tokens start a declaration
     * statement, aka {@code LET}.
     */
    public Ast.Statement.Declaration parseDeclarationStatement() throws
            ParseException {
        if(match("LET")) {
            if (peek(Token.Type.IDENTIFIER)) {
                String name = tokens.get(0).getLiteral();
                match(Token.Type.IDENTIFIER);
                if (peek("=")) {
                    match("=");
                    Optional<Ast.Expression> expr2 =
                            Optional.of(parseExpression());
                    if (match(";")) return new Ast.Statement.Declaration(name,
                            expr2);
                    else if (!peek(";")) {
                        int errorIndex = tokens.has(0) ? tokens.get(0).getIndex() :
                                (tokens.get(-1).getIndex() +
                                        tokens.get(-1).getLiteral().length());
                        throw new ParseException("Expected Semicolon at: " +
                                errorIndex, errorIndex);
                    }
                }
                if (peek(";")) return new Ast.Statement.Declaration(name,
                        Optional.empty());
                else if (!peek(";")) {
                    int errorIndex = tokens.has(0) ? tokens.get(0).getIndex() :
                            (tokens.get(-1).getIndex() +
                                    tokens.get(-1).getLiteral().length());
                    throw new ParseException("Expected Semicolon at: " +
                            errorIndex, errorIndex);
                }
            }
        }
        int errorIndex = tokens.has(0) ? tokens.get(0).getIndex() :
                (tokens.get(-1).getIndex() +
                        tokens.get(-1).getLiteral().length());
        throw new ParseException("Invalid Declaration Statement at: " + errorIndex,
                errorIndex);
    }
    /**
     * Parses an if statement from the {@code statement} rule. This method
     * should only be called if the next tokens start an if statement, aka
     * {@code IF}.
     */
    public Ast.Statement.If parseIfStatement() throws ParseException {
        if(match("IF")) {
            Ast.Expression condition = parseExpression();
            if (peek("DO")) {
                match("DO");
                List<Ast.Statement> thenStatements = new
                        ArrayList<Ast.Statement>();
                List<Ast.Statement> elseStatements = new
                        ArrayList<Ast.Statement>();
                boolean isThen = true;
                while (!peek("END")) {
                    if (isThen) thenStatements.add(parseStatement());
                    else elseStatements.add(parseStatement());
                    if (peek("ELSE")) {
                        isThen = false;
                        match("ELSE");
                    }
                }
                if (peek("END")) {
                    match("END");
                    return new Ast.Statement.If(condition, thenStatements,
                            elseStatements);
                } else if (!peek("END")) {
                    int errorIndex = tokens.has(0) ? tokens.get(0).getIndex() :
                            (tokens.get(-1).getIndex() +
                                    tokens.get(-1).getLiteral().length());
                    throw new ParseException("Expected END Identifier at: " +
                            errorIndex, errorIndex);
                }
            }
        }
        int errorIndex = tokens.has(0) ? tokens.get(0).getIndex() :
                (tokens.get(-1).getIndex() +
                        tokens.get(-1).getLiteral().length());
        throw new ParseException("Invalid If Statement at: " + errorIndex,
                errorIndex);
    }
    /**
     * Parses a for statement from the {@code statement} rule. This method
     * should only be called if the next tokens start a for statement, aka
     * {@code FOR}.
     */
    public Ast.Statement.For parseForStatement() throws ParseException {
        if(match("FOR")) {
            if (peek("(")) {
                match("(");
                Ast.Expression initialization = null;
                Ast.Expression recieverInitialization = null;
                if (peek(Token.Type.IDENTIFIER)) {
                    recieverInitialization = parseExpression();

                    if (peek("=")) {
                        match("=");
                        initialization = parseExpression();
                    }
                }
                if (peek(";") || initialization != null) {
                    match(";"); // Will only match if it exists
                    Ast.Expression condition = parseExpression();

                    if (peek(";")) {
                        Ast.Expression increment = null;
                        Ast.Expression receiverIncrement = null;
                        match(";");
                        if (peek(Token.Type.IDENTIFIER)) {
                            receiverIncrement = parseExpression();
                            if (peek("=")) {
                                match("=");

                                increment = parseExpression();
                            }

                        }
                        if (peek(")")) {

                            match(")");
                            List<Ast.Statement> forStatements = new
                                    ArrayList<Ast.Statement>();

                            while (!peek("END")) {
                                forStatements.add(parseStatement());
                            }

                            if (peek("END")) {
                                match("END");
                                return new Ast.Statement.For(new Ast.Statement.Assignment(recieverInitialization, initialization),
                                        condition, new Ast.Statement.Assignment(receiverIncrement, increment), forStatements);
                            } else if (!peek("END")) {
                                int errorIndex = tokens.has(0) ?
                                        tokens.get(0).getIndex() :
                                        (tokens.get(-1).getIndex() +
                                                tokens.get(-1).getLiteral().length());
                                throw new ParseException("Expected END Identifier at: " + errorIndex, errorIndex);
                            }
                        }
                        else if (!peek(")"))
                        {
                            int errorIndex = tokens.has(0) ?
                                    tokens.get(0).getIndex() :
                                    (tokens.get(-1).getIndex() +
                                            tokens.get(-1).getLiteral().length());
                            throw new ParseException("Expected Closing Parenthesis at: " + errorIndex, errorIndex);
                        }
                    } else if (!peek(";")) {
                        System.out.println("Is this it");
                        int errorIndex = tokens.has(0) ? tokens.get(0).getIndex() :
                                (tokens.get(-1).getIndex() +
                                        tokens.get(-1).getLiteral().length());
                        throw new ParseException("Expected Semicolon at: " +
                                errorIndex, errorIndex);
                    }
                } else if (!peek(";")) {
                    int errorIndex = tokens.has(0) ? tokens.get(0).getIndex() :
                            (tokens.get(-1).getIndex() +
                                    tokens.get(-1).getLiteral().length());
                    throw new ParseException("Expected Semicolon at: " +
                            errorIndex, errorIndex);
                }
            }
        }
        int errorIndex = tokens.has(0) ? tokens.get(0).getIndex() :
                (tokens.get(-1).getIndex() +
                        tokens.get(-1).getLiteral().length());
        throw new ParseException("Invalid For Statement at: " + errorIndex,
                errorIndex);
    }
    /**
     * Parses a while statement from the {@code statement} rule. This method
     * should only be called if the next tokens start a while statement, aka
     * {@code WHILE}.
     */
    public Ast.Statement.While parseWhileStatement() throws ParseException {
        if(match("WHILE")) {
            Ast.Expression condition = parseExpression();
            if (peek("DO")) {
                match("DO");
                List<Ast.Statement> whileStatements = new
                        ArrayList<Ast.Statement>();
                while (!peek("END")) {
                    whileStatements.add(parseStatement());
                }
                if (peek("END")) {
                    match("END");
                    return new Ast.Statement.While(condition, whileStatements);
                } else if (!peek("END"))
                {
                    int errorIndex = tokens.has(0) ? tokens.get(0).getIndex() :
                            (tokens.get(-1).getIndex() +
                                    tokens.get(-1).getLiteral().length());
                    throw new ParseException("Expected END Identifier at: " +
                            errorIndex, errorIndex);
                }
            } else if (!peek("DO")) {
                int errorIndex = tokens.has(0) ? tokens.get(0).getIndex() :
                        (tokens.get(-1).getIndex() +
                                tokens.get(-1).getLiteral().length());
                throw new ParseException("Expected DO Identifier at: " +
                        errorIndex, errorIndex);
            }
        }
        throw new ParseException("Invalid While Statement",
                tokens.get(-1).getIndex());
    }
    /**
     * Parses a return statement from the {@code statement} rule. This method
     * should only be called if the next tokens start a return statement, aka
     * {@code RETURN}.
     */
    public Ast.Statement.Return parseReturnStatement() throws ParseException {
        if(match("RETURN")) {
            Ast.Expression expr = parseExpression();
            if (peek(";")) {
                match(";");
                return new Ast.Statement.Return(expr);
            }
        }
        int errorIndex = tokens.has(0) ? tokens.get(0).getIndex() :
                (tokens.get(-1).getIndex() +
                        tokens.get(-1).getLiteral().length());
        throw new ParseException("Expected Semicolon at: " + errorIndex,
                errorIndex);
    }
    /**
     * Parses the {@code expression} rule.
     */
    public Ast.Expression parseExpression() throws ParseException {
        Ast.Expression expr = parseLogicalExpression();
        return expr;
    }
    /**
     * Parses the {@code logical-expression} rule.
     */
    public Ast.Expression parseLogicalExpression() throws ParseException {
        Ast.Expression expr = parseEqualityExpression();
        while(peek("AND") || peek("OR"))
        {
            String operator = tokens.get(0).getLiteral();
            match(operator);
            if (!tokens.has(0)) {
                int errorIndex = tokens.has(0) ? tokens.get(0).getIndex() :
                        (tokens.get(-1).getIndex() +
                                tokens.get(-1).getLiteral().length());
                throw new ParseException("Missing operand after '" + operator +
                        "'",
                        errorIndex);
            }
            Ast.Expression expr2 = parseEqualityExpression();
            expr = new Ast.Expression.Binary(operator, expr, expr2);
        }
        return expr;
    }

    /**
     * Parses the {@code equality-expression} rule.
     */

    public Ast.Expression parseEqualityExpression() throws ParseException {
        Ast.Expression expr = parseAdditiveExpression();
        while(peek("<") || peek("<=") ||
                peek(">") || peek(">=") ||
                peek("!=") || peek("=="))
        {
            String operator = tokens.get(0).getLiteral();
            match(operator);
            if (!tokens.has(0)) {
                int errorIndex = tokens.has(0) ? tokens.get(0).getIndex() :
                        (tokens.get(-1).getIndex() +
                                tokens.get(-1).getLiteral().length());
                throw new ParseException("Missing operand after '" + operator +
                        "'",
                        errorIndex);
            }
            Ast.Expression expr2 = parseAdditiveExpression();
            expr = new Ast.Expression.Binary(operator, expr, expr2);
        }
        return expr;
    }
    /**
     * Parses the {@code additive-expression} rule.
     */
    public Ast.Expression parseAdditiveExpression() throws ParseException {
        Ast.Expression expr = parseMultiplicativeExpression();
        while(peek("+") || peek("-"))
        {
            String operator = tokens.get(0).getLiteral();
            match(operator);
            if (!tokens.has(0)) {
                int errorIndex = tokens.has(0) ? tokens.get(0).getIndex() :
                        (tokens.get(-1).getIndex() +
                                tokens.get(-1).getLiteral().length());
                throw new ParseException("Missing operand after '" + operator +
                        "'",
                        errorIndex);
            }
            Ast.Expression expr2 = parseMultiplicativeExpression();
            expr = new Ast.Expression.Binary(operator, expr, expr2);
        }
        return expr;
    }
    /**
     * Parses the {@code multiplicative-expression} rule.
     */
    public Ast.Expression parseMultiplicativeExpression() throws ParseException {
        Ast.Expression expr = parseSecondaryExpression();
        while(peek("*") || peek("/"))
        {
            String operator = tokens.get(0).getLiteral();
            match(operator);
            if (!tokens.has(0)) {
                int errorIndex = tokens.has(0) ? tokens.get(0).getIndex() :
                        (tokens.get(-1).getIndex() +
                                tokens.get(-1).getLiteral().length());
                throw new ParseException("Missing operand after '" + operator +
                        "'",
                        errorIndex);
            }
            Ast.Expression expr2 = parseSecondaryExpression();
            expr = new Ast.Expression.Binary(operator, expr, expr2);
        }
        return expr;
    }

    /**
     * Parses the {@code secondary-expression} rule.
     */
    public Ast.Expression parseSecondaryExpression() throws ParseException {
        Ast.Expression expr = parsePrimaryExpression();
        Optional<Ast.Expression> receiver = Optional.empty();
        receiver = Optional.of(new Ast.Expression.Access(Optional.empty(), tokens.get(-1).getLiteral()));

        while(peek(".")) {
            match(".");
            if(peek(Token.Type.IDENTIFIER))
            {
                String name = tokens.get(0).getLiteral();
                match(Token.Type.IDENTIFIER);
                List <Ast.Expression> arguments = new ArrayList<>();
                if(peek("("))
                {
                    match("(");
                    if(!peek(")"))
                        arguments.add(parsePrimaryExpression());
                    while(peek(","))
                    {
                        match(",");
                        arguments.add(parsePrimaryExpression());
                    }
                    if(peek(")")) match(")");
                    else {
                        int errorIndex = tokens.has(0) ? tokens.get(0).getIndex() :
                                (tokens.get(-1).getIndex() +
                                        tokens.get(-1).getLiteral().length());
                        throw new ParseException("Expected ')' Operator at: "
                                + errorIndex, errorIndex);
                    }
                    if(peek("."))
                        receiver = Optional.of(new Ast.Expression.Function(receiver, name, arguments));
                    else
                    {
                        expr = new Ast.Expression.Function(receiver, name, arguments);
                    }
                }
                else {
                    if(peek("."))
                        receiver = Optional.of(new Ast.Expression.Access(receiver, name));
                    else {
                        expr = new Ast.Expression.Access(receiver, name);
                    }
                }

            }
            else {
                int errorIndex = tokens.has(0) ? tokens.get(0).getIndex() :
                        (tokens.get(-1).getIndex() +
                                tokens.get(-1).getLiteral().length());

                throw new ParseException("Expected Identifier after the '.' at: "
                        + errorIndex, errorIndex);
            }
        }

        return expr;
    }
    /**
     * Parses the {@code primary-expression} rule. This is the top-level rule
     * for expressions and includes literal values, grouping, variables, and
     * functions. It may be helpful to break these up into other methods but is
     * not strictly necessary.
     */
    public Ast.Expression parsePrimaryExpression() throws ParseException {
        String _nil = "NIL";
        String _true = "TRUE";
        String _false = "FALSE";
        if (peek(_nil)) {
            match(_nil);
            return new Ast.Expression.Literal(null);
        } else if (peek(_true)) {
            match(_true);
            return new Ast.Expression.Literal(true);
        } else if (peek(_false)) {
            match(_false);
            return new Ast.Expression.Literal(false);
        } else if (peek(Token.Type.INTEGER)) {
            match(Token.Type.INTEGER);
            return new Ast.Expression.Literal(new
                    BigInteger(tokens.get(-1).getLiteral()));
        } else if (peek(Token.Type.DECIMAL)) {
            match(Token.Type.DECIMAL);
            return new Ast.Expression.Literal(new
                    BigDecimal(tokens.get(-1).getLiteral()));
        } else if (peek(Token.Type.CHARACTER)) {
            match(Token.Type.CHARACTER);
            String processed = tokens.get(-1).getLiteral();
            processed = processed.replaceAll("^'", "")
                    .replaceAll("'$", "")
                    .replace("\\'", "'")
                    .replace("\\\"", "\"")
                    .replace("\\t", "\t")
                    .replace("\\r", "\r")
                    .replace("\\n", "\n")
                    .replace("\\b", "\b");
            return new Ast.Expression.Literal(processed.charAt(0));
        } else if (peek(Token.Type.STRING)) {
            match(Token.Type.STRING);
            String processed = tokens.get(-1).getLiteral();
            processed = processed.replaceAll("^\"", "")
                    .replaceAll("\"$", "")
                    .replace("\\n", "\n")
                    .replace("\\t", "\t")
                    .replace("\\r", "\r")
                    .replace("\\'", "'")
                    .replace("\\\\", "\\")
                    .replace("\\\"", "\"")
                    .replace("\\b", "\b");
            return new Ast.Expression.Literal(processed);
        } else if (peek("(")) {
            match("(");
            Ast.Expression expr = parseExpression();
            if (match(")")) return new Ast.Expression.Group(expr);
            int errorIndex = tokens.has(0) ? tokens.get(0).getIndex() :
                    (tokens.get(-1).getIndex() +
                            tokens.get(-1).getLiteral().length());
            throw new ParseException("Expected Closing Parenthesis at: " +
                    errorIndex, errorIndex);
        } else if (peek(Token.Type.IDENTIFIER)) {

            String name = tokens.get(0).getLiteral();
            match(Token.Type.IDENTIFIER);
            if (peek("(")) {
                match("(");
                if (!peek(")")) {
                    List<Ast.Expression> arguments = new
                            ArrayList<Ast.Expression>();
                    arguments.add(parseExpression());
                    while (peek(",")) {
                        match(",");
                        arguments.add(parseExpression());
                    }
                    if (tokens.get(-1).getLiteral().equals(",")) {
                        int errorIndex = tokens.has(0) ? tokens.get(0).getIndex() :
                                (tokens.get(-1).getIndex() +
                                        tokens.get(-1).getLiteral().length());
                        throw new ParseException("Trailing Comma Error at: " +
                                errorIndex, errorIndex);
                    }
                    if (peek(")")) {
                        match(")");
                        return new Ast.Expression.Function(Optional.empty(), name,
                                arguments);
                    }
                    int errorIndex = tokens.has(0) ? tokens.get(0).getIndex() :
                            (tokens.get(-1).getIndex() +
                                    tokens.get(-1).getLiteral().length());
                    throw new ParseException("Expected Closing Parenthesis at: " +
                            errorIndex, errorIndex);
                }
                else {
                    match(")");
                    return new Ast.Expression.Function(Optional.empty(), name, new
                            ArrayList<Ast.Expression>());
                }
            }
            else return new Ast.Expression.Access(Optional.empty(),
                    tokens.get(-1).getLiteral());
        }
        int errorIndex = tokens.has(0) ? tokens.get(0).getIndex() :
                (tokens.get(-1).getIndex() +
                        tokens.get(-1).getLiteral().length());
        throw new ParseException("Invalid Primary Expression at: " + errorIndex,
                errorIndex);
    }
    /**
     * As in the lexer, returns {@code true} if the current sequence of tokens
     * matches the given patterns. Unlike the lexer, the pattern is not a regex;
     * instead it is either a {@link Token.Type}, which matches if the token's
     * type is the same, or a {@link String}, which matches if the token's
     * literal is the same.
     *
     * In other words, {@code Token(IDENTIFIER, "literal")} is matched by both
     * {@code peek(Token.Type.IDENTIFIER)} and {@code peek("literal")}.
     */
    private boolean peek(Object... patterns) {
        for(int i = 0; i < patterns.length;i++)
        {
            if(!tokens.has(i))
            {
                return false;
            }
            else if (patterns[i] instanceof Token.Type)
            {
                if(patterns[i] != tokens.get(i).getType())
                {
                    return false;
                }
            }
            else if (patterns[i] instanceof String)
            {
                if(!patterns[i].equals(tokens.get(i).getLiteral()))
                {
                    return false;
                }
            }
            else
            {
                throw new AssertionError("Invalid pattern object: " +
                        patterns[i].getClass());
            }
        }
        return true;
    }
    /**
     * As in the lexer, returns {@code true} if {@link #peek(Object...)} is true
     * and advances the token stream.
     */
    private boolean match(Object... patterns) {
        boolean peek = peek(patterns);
        if(peek)
        {
            for(int i = 0; i < patterns.length; i++)
            {
                tokens.advance();
            }
        }
        return peek;
    }
    private static final class TokenStream {
        private final List<Token> tokens;
        private int index = 0;
        private TokenStream(List<Token> tokens) {
            this.tokens = tokens;
        }
        /**
         * Returns true if there is a token at index + offset.
         */
        public boolean has(int offset) {
            return index + offset < tokens.size();
        }
        /**
         * Gets the token at index + offset.
         */
        public Token get(int offset) {
            return tokens.get(index + offset);
        }
        /**
         * Advances to the next token, incrementing the index.
         */
        public void advance() {
            index++;
        }
    }
}