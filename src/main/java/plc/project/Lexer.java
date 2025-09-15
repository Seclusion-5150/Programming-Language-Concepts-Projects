package plc.project;

import java.util.ArrayList;
import java.util.List;

/**
 * The lexer works through three main functions:
 *
 *  - {@link #lex()}, which repeatedly calls lexToken() and skips whitespace
 *  - {@link #lexToken()}, which lexes the next token
 *  - {@link CharStream}, which manages the state of the lexer and literals
 *
 * If the lexer fails to parse something (such as an unterminated string) you
 * should throw a {@link ParseException} with an index at the invalid character.
 *
 * The {@link #peek(String...)} and {@link #match(String...)} functions are
 * helpers you need to use, they will make the implementation easier.
 */
public final class Lexer {

    private final CharStream chars;


    public Lexer(String input) {
        chars = new CharStream(input);
//        identifierPattern = "[A-Za-z_] [A-Za-z0-9_-]*";
//        numberPattern = "[+-]?[0-9]+(\\.[0-9]+)?";
//        characterPattern = "'([^'\\n\\r\\\\]|\\\\[bnrt'\"\\\\])'";
//        stringPattern = "\"([^\"\\n\\r\\\\]|\\\\[bnrt'\"\\\\])*\"";
//        escapePattern = "\\\\[bnrt'\"\\\\]";
//        operatorPattern = "[<>!=]=?|.";
//        whitespacePattern = "[ \\b\\n\\r\\t]";
    }

    /**
     * Repeatedly lexes the input using {@link #lexToken()}, also skipping over
     * whitespace where appropriate.
     */
    public List<Token> lex() {
        List<Token> tokens = new ArrayList<Token>();
        String whitespacePattern = "[ \\u0008\\n\\r\\t]";

        while(chars.has(0)) {
            while(match(whitespacePattern));
            chars.skip();
            if(!chars.has(0)) break;
            Token lexed_token = lexToken();
            if(lexed_token == null) throw new ParseException("Error Parse Exception", chars.index);
            tokens.add(lexed_token);

        }

        return tokens;
    }

    /**
     * This method determines the type of the next token, delegating to the
     * appropriate lex method. As such, it is best for this method to not change
     * the state of the char stream (thus, use peek not match).
     *
     * The next character should start a valid token since whitespace is handled
     * by {@link #lex()}
     */

    public Token lexToken() {

        String identifierPatternStart = "[A-Za-z_]";
        String numberPatternStart0 = "[0-9]";
        String numberPatternStart1 = "[+-]";
        String numberPatternStart2 = "[1-9]";
        String characterPatternStart = "'";
        String stringPatternStart = "\"";
        String escapePatternStart = "\\\\";
        Token lexed_token = null;

        if(peek(identifierPatternStart)) lexed_token = lexIdentifier();
        else if(peek(numberPatternStart1, numberPatternStart2) || peek(numberPatternStart0)) lexed_token = lexNumber();
        else if(peek(characterPatternStart)) lexed_token = lexCharacter();
        else if(peek(stringPatternStart)) lexed_token = lexString();
        else if(peek(escapePatternStart)) lexEscape();
        else lexed_token = lexOperator();

        return lexed_token;
    }

    public Token lexIdentifier() {
        String identifierPattern = "[A-Za-z0-9_-]";
        while(match(identifierPattern));

        return chars.emit(Token.Type.IDENTIFIER);
    }

    public Token lexNumber() {

        String numberPattern1 = "[0-9]";
        String numberPattern2 = "\\.";
        if(match("[0]", numberPattern1)) throw new ParseException("Invalid Integer", chars.index);

        boolean pattern1 = true;
        boolean pattern2 = true;
        boolean isDecimal = false;

        if(match("[+-]", "[0-9]", "\\.", "[0-9]")) isDecimal = true;
        else match("[+-]", "[1-9]");

        while(match(numberPattern1));

        if(peek(numberPattern2, numberPattern1) && !isDecimal) {
            match(numberPattern2);
            isDecimal = true;
            while (match(numberPattern1)) ;
        }

        return isDecimal? chars.emit(Token.Type.DECIMAL) : chars.emit(Token.Type.INTEGER);
    }

    public Token lexCharacter() {
        String characterPattern0 = "'";
        String characterPattern1 = "[^'\\n\\r\\\\]";
        String characterPattern2 = "\\\\";
        String characterPattern3 = "[bnrt'\"\\\\]";

        int startIndex = chars.index;

        if(match(characterPattern0)) {
            if(match(characterPattern1)) {
                if(match(characterPattern0)) {
                    return chars.emit(Token.Type.CHARACTER);
                } else {
                    throw new ParseException("Unterminated character", chars.index);
                }
            } else if(match(characterPattern2)) {
                if(match(characterPattern3)) {
                    if(match(characterPattern0)) {
                        return chars.emit(Token.Type.CHARACTER);
                    }
                    else {
                        throw new ParseException("Unterminated character", chars.index);
                    }
                }
                else {
                    throw new ParseException("Invalid escape sequence", chars.index);
                }
            }
            else {
                throw new ParseException("Invalid character content", chars.index);
            }
        }

        throw new ParseException("Invalid character", startIndex);
    }

    public Token lexString() {
        String stringPattern0 = "\"";
        String stringPattern1 = "[^\"\\n\\r\\\\]";
        String stringPattern2 = "\\\\";
        String stringPattern3 = "[bnrt'\"\\\\]";

        match(stringPattern0);

        while(chars.has(0) && !peek(stringPattern0)) {
            if(match(stringPattern1)) continue;
            else if(match(stringPattern2)) {
                if(!match(stringPattern3)) throw new ParseException("Invalid Escape Sequence", chars.index);
            }
            else throw new ParseException("Invalid character in string", chars.index);
        }

        if(match(stringPattern0)) return chars.emit(Token.Type.STRING);

        throw new ParseException("Unterminated String", chars.index);
    }
    public void lexEscape() {
        String escapePattern0 = "\\\\";
        String escapePattern1 = "[bnrt'\"\\\\]";
        match(escapePattern0);
        match(escapePattern1);
        chars.skip();
    }

    public Token lexOperator() {
        String operatorPattern0 = "[<>!=]";
        String operatorPattern1 = "=";

        if(match(operatorPattern0)) {
            match(operatorPattern1);
            return chars.emit(Token.Type.OPERATOR);
        }

        match(".");

        return chars.emit(Token.Type.OPERATOR);
    }

    /**
     * Returns true if the next sequence of characters match the given patterns,
     * which should be a regex. For example, {@code peek("a", "b", "c")} would
     * return true if the next characters are {@code 'a', 'b', 'c'}.
     */
    public boolean peek(String... patterns) {
        for(int i = 0; i < patterns.length;i++)
        {
            if(!chars.has(i) || !String.valueOf(chars.get(i)).matches(patterns[i]))
            {
                return false;
            }
        }
        return true;
    }

    /**
     * Returns true in the same way as {@link #peek(String...)}, but also
     * advances the character stream past all matched characters if peek returns
     * true. Hint - it's easiest to have this method simply call peek.
     */
    public boolean match(String... patterns) {
        boolean peek = peek(patterns);
        if(peek)
        {
            for(int i = 0; i < patterns.length;i++)
            {
                chars.advance();
            }
        }
        return peek;
    }

    /**
     * A helper class maintaining the input string, current index of the char
     * stream, and the current length of the token being matched.
     *
     * You should rely on peek/match for state management in nearly all cases.
     * The only field you need to access is {@link #index} for any {@link
     * ParseException} which is thrown.
     */

    public static final class CharStream {

        private final String input;
        private int index = 0;
        private int length = 0;

        public CharStream(String input) {
            this.input = input;
        }

        public boolean has(int offset) {
            return index + offset < input.length();
        }

        public char get(int offset) {
            return input.charAt(index + offset);
        }

        public void advance() {
            index++;
            length++;
        }

        public void skip() {
            length = 0;
        }

        public Token emit(Token.Type type) {
            int start = index - length;
            skip();
            return new Token(type, input.substring(start, index), start);
        }

    }

}
