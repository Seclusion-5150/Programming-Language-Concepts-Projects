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
            if(peek("LET")) {
                match("LET");
                fields.add(parseField());
            }
            if(peek("DEF"))
            {
                match("DEF");
                methods.add(parseMethod());
            }

        }


        return new Ast.Source(fields, methods);
    }


    /**
     * Parses the {@code field} rule. This method should only be called if the
     * next tokens start a field, aka {@code LET}.
     */
    public Ast.Field parseField() throws ParseException {
        boolean constant = false;
        if(peek("CONST"))
        {
            match("CONST");
            constant = true;
        }
        if(peek(Token.Type.IDENTIFIER))
        {
            String name = tokens.get(0).getLiteral();
            match(Token.Type.IDENTIFIER);
            if(peek("="))
            {
                match("=");
                Optional<Ast.Expression> expr = Optional.of(parseExpression());
                if(peek(";"))
                {
                    match(";");
                    return new Ast.Field(name, constant, expr);
                }
                else if(!peek(";"))
                {
                    throw new ParseException("Expected Semicolon at: ", tokens.get(0).getIndex());
                }

            }

            if(peek(";"))
            {
                match(";");

                return new Ast.Field(name, constant,Optional.empty());
            }
            else if(!peek(";"))
            {
                throw new ParseException("Expected Semicolon at: ", tokens.get(0).getIndex());
            }
        }
        throw new ParseException("Invalid Field at: ", tokens.get(0).getIndex()); //TODO
    }

    /**
     * Parses the {@code method} rule. This method should only be called if the
     * next tokens start a method, aka {@code DEF}.
     */
    public Ast.Method parseMethod() throws ParseException {
        if(peek(Token.Type.IDENTIFIER))
        {
            String name = tokens.get(0).getLiteral();
            match(Token.Type.IDENTIFIER);
            if(peek("("))
            {
                match("(");
                List<String> parameters = new ArrayList<String>();
                if(peek(Token.Type.IDENTIFIER)) {
                    parameters.add(tokens.get(0).getLiteral());
                    match(Token.Type.IDENTIFIER);
                    while(peek(","))
                    {
                        match(",");
                        if(peek(Token.Type.IDENTIFIER)) {
                            parameters.add(tokens.get(0).getLiteral());
                            match(Token.Type.IDENTIFIER);
                        }
                    }
                }
                if(peek(")"))
                {
                    match(")");

                    if(peek("DO"))
                    {
                        match("DO");
                        List<Ast.Statement> methodStatements = new ArrayList<Ast.Statement>();

                        while(!peek("END"))
                        {
                            methodStatements.add(parseStatement());
                        }
                        if(peek("END"))
                        {

                            match("END");
                            return new Ast.Method(name, parameters, methodStatements);
                        }
                        else if(!peek("END"))
                        {
                            throw new ParseException("Missing END Idenfitier at", tokens.get(-1).getIndex());
                        }
                    }
                    else if(!peek("DO"))
                    {
                        throw new ParseException("Missing DO Idenfitier at", tokens.get(-1).getIndex());
                    }
                }
                else if(!peek(")"))
                {
                    throw new ParseException("Expected Closing Parenthesis at: ", tokens.get(-1).getIndex());
                }
            }
        }

        throw new ParseException("Invalid Method", tokens.get(0).getIndex());
    }

    /**
     * Parses the {@code statement} rule and delegates to the necessary method.
     * If the next tokens do not start a declaration, if, for, while, or return
     * statement, then it is an expression/assignment statement.
     */
    public Ast.Statement parseStatement() throws ParseException {
        // First we have to determine what kind of statement the token is;

        if (peek("LET"))
        {
            match("LET");
            return parseDeclarationStatement();
        }
        else if(peek("IF"))
        {
            match("IF");
            return parseIfStatement();
        }
        else if(peek("FOR"))
        {
            match("FOR");
            return parseForStatement();
        }
        else if(peek("WHILE"))
        {
            match("WHILE");
            return parseWhileStatement();
        }
        else if(peek("RETURN"))
        {
            match("RETURN");
            return parseReturnStatement();
        }
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
                else if (!peek(";")) throw new ParseException("Expected Semicolon at: ", tokens.get(0).getIndex());
            }
            if(peek(";")) {
                match(";");
                return new Ast.Statement.Expression(expr);
            }
            else if (!peek(";")) throw new ParseException("Expected Semicolon at: ", tokens.get(-1).getIndex());

        }
        throw new ParseException("Invalid Parse Statement at: ", tokens.get(0).getIndex());
    }

    /**
     * Parses a declaration statement from the {@code statement} rule. This
     * method should only be called if the next tokens start a declaration
     * statement, aka {@code LET}.
     */
    public Ast.Statement.Declaration parseDeclarationStatement() throws ParseException {

        if(peek(Token.Type.IDENTIFIER))
        {
            String name = tokens.get(0).getLiteral();
            match(Token.Type.IDENTIFIER);
            if (peek("="))
            {
                match("=");

                Optional<Ast.Expression> expr2 = Optional.of(parseExpression());
                if(match(";")) return new Ast.Statement.Declaration(name, expr2);
                else if (!peek(";")) throw new ParseException("Expected Semicolon at: ", tokens.get(-1).getIndex());

            }

            if(peek(";")) return new Ast.Statement.Declaration(name, Optional.empty());
            else if (!peek(";")) throw new ParseException("Expected Semicolon at: ", tokens.get(-1).getIndex());

        }
        throw new ParseException("Invalid Declaration Statement at: ", tokens.get(0).getIndex());
    }

    /**
     * Parses an if statement from the {@code statement} rule. This method
     * should only be called if the next tokens start an if statement, aka
     * {@code IF}.
     */
    public Ast.Statement.If parseIfStatement() throws ParseException {
        Ast.Expression condition = parseExpression();
        if(peek("DO"))
        {
            match("DO");
            List<Ast.Statement> thenStatements = new ArrayList<Ast.Statement>();
            List<Ast.Statement> elseStatements = new ArrayList<Ast.Statement>();

            boolean isThen = true;

            while(!peek("END"))
            {

                if(isThen) thenStatements.add(parseStatement());
                else elseStatements.add(parseStatement());

                if(peek("ELSE")) {
                    isThen = false;
                    match("ELSE");
                }
            }
            if(peek("END"))
            {
                match("END");
                return new Ast.Statement.If(condition, thenStatements, elseStatements);
            }
            else if (!peek("END")) throw new ParseException("Expected END Identifier at: ", tokens.get(-1).getIndex());

        }
        throw new ParseException("Invalid If Statement at: ", tokens.get(-1).getIndex());
    }

    /**
     * Parses a for statement from the {@code statement} rule. This method
     * should only be called if the next tokens start a for statement, aka
     * {@code FOR}.
     */
    public Ast.Statement.For parseForStatement() throws ParseException {
        if(peek("(")) {
            match("(");
            Ast.Statement initialization = null;
            if(peek(Token.Type.IDENTIFIER))
            {
                match(Token.Type.IDENTIFIER);
                if(peek("="))
                {
                    match("=");
                    initialization = parseStatement();
                }
            }
            if(peek(";"))
            {
                match(";");
                Ast.Expression condition = parseExpression();

                if(peek(";"))
                {
                    Ast.Statement increment = null;
                    match(";");
                    if(peek(Token.Type.IDENTIFIER))
                    {
                        match(Token.Type.IDENTIFIER);
                        if(peek("="))
                        {
                            match("=");
                            increment = parseStatement();
                        }
                    }
                    if(peek(")"))
                    {
                        match(")");
                        List<Ast.Statement> forStatements = new ArrayList<Ast.Statement>();
                        while(!peek("END"))
                        {
                            forStatements.add(parseStatement());
                        }
                        if(peek("END"))
                        {
                            match("END");
                            return new Ast.Statement.For(initialization, condition, increment, forStatements);
                        }
                        else if (!peek("END")) throw new ParseException("Expected END Identifier at: ", tokens.get(-1).getIndex());
                    }
                    else if (!peek(")")) throw new ParseException("Expected Closing Parenthesis at: ", tokens.get(-1).getIndex());

                }
                else if (!peek(";")) throw new ParseException("Expected Semicolon at: ", tokens.get(-1).getIndex());

            }
            else if (!peek(";")) throw new ParseException("Expected Semicolon at: ", tokens.get(-1).getIndex());

        }
        throw new ParseException("Invalid For Statement at: ", tokens.get(0).getIndex());
    }

    /**
     * Parses a while statement from the {@code statement} rule. This method
     * should only be called if the next tokens start a while statement, aka
     * {@code WHILE}.
     */
    public Ast.Statement.While parseWhileStatement() throws ParseException {
        Ast.Expression condition = parseExpression();
        if(peek("DO"))
        {
            match("DO");
            List<Ast.Statement> whileStatements = new ArrayList<Ast.Statement>();
            while(!peek("END"))
            {
                whileStatements.add(parseStatement());
            }
            if(peek("END")) {
                match("END");

                return new Ast.Statement.While(condition, whileStatements);
            }
            else if (!peek("END")) throw new ParseException("Expected END Identifier at: ", tokens.get(-1).getIndex());
        }
        else if (!peek("DO")) throw new ParseException("Expected DO Identifier at: ", tokens.get(-1).getIndex());

        throw new ParseException("Invalid While Statement", tokens.get(-1).getIndex());
    }

    /**
     * Parses a return statement from the {@code statement} rule. This method
     * should only be called if the next tokens start a return statement, aka
     * {@code RETURN}.
     */
    public Ast.Statement.Return parseReturnStatement() throws ParseException {

        Ast.Expression expr = parseExpression();
        if(peek(";")) {
            match(";");
            return new Ast.Statement.Return(expr);
        }
        throw new ParseException("Expected Semicolon at: ", tokens.get(-1).getIndex());
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

        while(peek("AND") || peek("OR") || peek("&&") || peek("||"))
        {
            String operator = tokens.get(0).getLiteral();
            match(operator);
            Ast.Expression expr2 = parseMultiplicativeExpression();
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
            Ast.Expression expr2 = parseMultiplicativeExpression();
            expr = new Ast.Expression.Binary(operator, expr, expr2);
        }

        return expr;
    }

    /**
     * Parses the {@code secondary-expression} rule.
     */
    public Ast.Expression parseSecondaryExpression() throws ParseException {
        Ast.Expression expr = parsePrimaryExpression();
        if(peek(".")) {
            String name1 = tokens.get(-1).getLiteral();
            match(".");

            if (peek(Token.Type.IDENTIFIER)) {
                match(Token.Type.IDENTIFIER);
                if(peek("("))
                {
                    String name2 = tokens.get(-1).getLiteral();
                    match("(");
                    List<Ast.Expression> secondaryArguments = new ArrayList<Ast.Expression>();

                    secondaryArguments.add(parseExpression());
                    while(peek(","))
                    {
                        match(",");
                        secondaryArguments.add(parseExpression());
                    }
                    if(peek(")"))
                    {
                        secondaryArguments.removeIf(element -> element instanceof Ast.Expression.Literal);
                        Optional<Ast.Expression> method = Optional.of(new Ast.Expression.Access(Optional.empty(), name1));
                        return new Ast.Expression.Function(method, name2, secondaryArguments);
                    }
                }

                Optional<Ast.Expression> receiver = Optional.of(expr);

                return new Ast.Expression.Access(receiver, tokens.get(-1).getLiteral());
            }
            throw new ParseException("Identifier Expected After '.' Operator.", tokens.get(-1).getIndex());
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
            return new Ast.Expression.Literal(new BigInteger(tokens.get(-1).getLiteral()));
        } else if (peek(Token.Type.DECIMAL)) {
            match(Token.Type.DECIMAL);
            return new Ast.Expression.Literal(new BigDecimal(tokens.get(-1).getLiteral()));
        } else if (peek(Token.Type.CHARACTER)) {
            match(Token.Type.CHARACTER);
            String processed = tokens.get(-1).getLiteral();
            processed = processed.replace("\\n", "\n")
                    .replace("\\t", "\t")
                    .replace("\\r", "\r")
                    .replace("'", "")
                    .replace("\\", "\"")
                    .replace("\"", "");

            return new Ast.Expression.Literal(processed.charAt(0));
        } else if (peek(Token.Type.STRING)) {
            match(Token.Type.STRING);
            String processed = tokens.get(-1).getLiteral();
            processed = processed.replace("\\n", "\n")
                    .replace("\\t", "\t")
                    .replace("\\r", "\r")
                    .replace("'", "")
                    .replace("\\", "\"")
                    .replace("\"", "");

            return new Ast.Expression.Literal(processed);
        } else if (peek("(")) {
            match("(");

            Ast.Expression expr = parseExpression();

            if (match(")")) return new Ast.Expression.Group(expr);

            throw new ParseException("Expected Closing Parenthesis at: ", tokens.get(-1).getIndex());

        } else if (peek(Token.Type.IDENTIFIER)) {
            String name = tokens.get(0).getLiteral();
            match(Token.Type.IDENTIFIER);

            if (peek("(")) {
                match("(");

                List<Ast.Expression> arguments = new ArrayList<Ast.Expression>();
                arguments.add(parseExpression());

                while (peek(",")) {
                    match(",");

                    arguments.add(parseExpression());

                }

                arguments.removeIf(element -> (element instanceof Ast.Expression.Literal));

                if (match(")")) {
                    return new Ast.Expression.Function(Optional.empty(), name, arguments);
                }
                throw new ParseException("Expected Closing Parenthesis at: ", tokens.get(-1).getIndex());
            }
            else return new Ast.Expression.Access(Optional.empty(), tokens.get(-1).getLiteral());
        }
        else if(peek(")"))
        {
            return new Ast.Expression.Literal("");
        }
        throw new ParseException("Expected Closing Parenthesis at: ", tokens.get(0).getIndex());
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
                throw new AssertionError("Invalid pattern object: " + patterns[i].getClass());
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
