package plc.homework;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.regex.Pattern;
import java.util.stream.Stream;

/**
 * Contains JUnit tests for {@link Regex}. A framework of the test structure 
 * is provided, you will fill in the remaining pieces.
 *
 * To run tests, either click the run icon on the left margin, which can be used
 * to run all tests or only a specific test. You should make sure your tests are
 * run through IntelliJ (File > Settings > Build, Execution, Deployment > Build
 * Tools > Gradle > Run tests using <em>IntelliJ IDEA</em>). This ensures the
 * name and inputs for the tests are displayed correctly in the run window.
 */

public class RegexTests {

    /**
     * This is a parameterized test for the {@link Regex#EMAIL} regex. The
     * {@link ParameterizedTest} annotation defines this method as a
     * parameterized test, and {@link MethodSource} tells JUnit to look for the
     * static method {@link #testEmailRegex()}.
     *
     * For personal preference, I include a test name as the first parameter
     * which describes what that test should be testing - this is visible in
     * IntelliJ when running the tests (see above note if not working).
     */
    @ParameterizedTest
    @MethodSource
    public void testEmailRegex(String test, String input, boolean success) {
        test(input, Regex.EMAIL, success);
    }

    /**
     * This is the factory method providing test cases for the parameterized
     * test above - note that it is static, takes no arguments, and has the same
     * name as the test. The {@link Arguments} object contains the arguments for
     * each test to be passed to the function above.
     */
    public static Stream<Arguments> testEmailRegex() {
        return Stream.of(
                // Matching
                Arguments.of("Two Letter Domain Extension", "robert@hotmail.ca", true),
                Arguments.of("Alphanumeric", "thelegend27@gmail.com", true),
                Arguments.of("UF Domain", "otherdomain@ufl.edu", true),
                Arguments.of("With Underscores", "user_name@domain.org", true),
                Arguments.of("With Dashes", "first-last@company.net", true),
                Arguments.of("With Periods", "first.last@site.com", true),
                Arguments.of("Numbers In Domain", "test@domain123.com", true),

                // Not Matching
                Arguments.of("Too many @'s", "manyats@@@gmail.com", false),
                Arguments.of("Too many .s", "otherdomain@ufl...edu", false),
                Arguments.of("Missing Domain Dot", "missingdot@gmailcom", false),
                Arguments.of("Symbols", "symbols#$%@gmail.com", false),
                Arguments.of("Missing @", "missingatgmail.com", false),
                Arguments.of("Dot At The End", "otherdomain@ufl.ed.", false),
                Arguments.of("Domain Extension Too Long", "user@domain.info", false),
                Arguments.of("Upper Case Domain Extension", "user@domain.COM", false)

                );
    }

    @ParameterizedTest
    @MethodSource
    public void testOddStringsRegex(String test, String input, boolean success) {
        test(input, Regex.ODD_STRINGS, success);
    }

    public static Stream<Arguments> testOddStringsRegex() {
        return Stream.of(
                // what have eleven letters and starts with gas? automobiles.
                //Matching
                Arguments.of("11 Characters", "automobiles", true),
                Arguments.of("13 Characters", "i<3pancakes13", true),
                Arguments.of("Abnormal Characters", "@\\(#_@#\\)\\(+;\\];&", true),
                Arguments.of("Upper-Bound: 19 Characters", "counterintelligence", true),
                Arguments.of("Upper-Bound: 15 Characters", "counterstriketo", true),

                // Not Matching
                Arguments.of("Out of Bounds: 20 Characters", "counterinterpretatio", false),
                Arguments.of("Out of Bounds: 21 Characters", "counterinterpretation", false),
                Arguments.of("10 Characters", "i<3pancake", false),
                Arguments.of("14 Characters", "i<3pancakes14!", false),
                Arguments.of("12 Characters", "wakeupwakeup", false),
                Arguments.of("Just Under Range: 9 Characters", "wakeupwak", false),
                Arguments.of("5 Characters", "5five", false)
        );
    }

    @ParameterizedTest
    @MethodSource
    public void testIntegerListRegex(String test, String input, boolean success) {
        test(input, Regex.INTEGER_LIST, success);
    }

    public static Stream<Arguments> testIntegerListRegex() {
        return Stream.of(
                // Matching
                Arguments.of("Empty List", "[]", true),
                Arguments.of("Single Element", "[1]", true),
                Arguments.of("Multiple Elements", "[1,20,3]", true),
                Arguments.of("Large Numbers", "[12345,2698120,123104809]", true),
                Arguments.of("Uneven spacing", "[1,2, 3]", true),

                // Not Matching
                Arguments.of("Missing Brackets", "1,2,3", false),
                Arguments.of("Missing Commas", "[1 2 3]", false),
                Arguments.of("Trailing Comma", "[1,2,3,]", false),
                Arguments.of("Open Bracket Right", "[1,2,3", false),
                Arguments.of("Open Bracket Left", "1,2,3]", false)
        );
    }

    @ParameterizedTest
    @MethodSource
    public void testDecimalRegex(String test, String input, boolean success) {
        test(input, Regex.DECIMAL, success);
    }

    public static Stream<Arguments> testDecimalRegex() {
        return Stream.of(
                // Not Matching
                Arguments.of("No Zero After \\.", "5.", false),
                Arguments.of("No Zero Before \\.", ".76", false),
                Arguments.of("Non Decimal", "25", false),
                Arguments.of("Leading Zeros Left", "00012.5", false),
                Arguments.of("Positive Symbol", "+1002000.005", false),
                Arguments.of("Spaces", "100 2000.0 05", false),
                Arguments.of("Only Leading Zeros", "0000.5", false),

                // Matching
                Arguments.of("Leading Zeros Right", "12.000005", true),
                Arguments.of("Only Zero Left", "0.3", true),
                Arguments.of("Negative", "-3.0", true),
                Arguments.of("Regular Decimal", "3.2", true),
                Arguments.of("Multiple Digits Left", "1312323.0", true),
                Arguments.of("Multiple Digits Right", "3.12312312", true),
                Arguments.of("Multiple Digits Both Sides", "1312323.1231312", true)
                );
    }

    @ParameterizedTest
    @MethodSource
    public void testStringRegex(String test, String input, boolean success) {
        test(input, Regex.STRING, success);
    }

    public static Stream<Arguments> testStringRegex() {
        return Stream.of(
                // Matching
                Arguments.of("Empty", "\"\"", true),
                Arguments.of("Length 1", "\"H\"", true),
                Arguments.of("Random Text", "\"afafasfas(*&(*&(*&asfflsflksns'a;d;la'd';.\"", true),
                Arguments.of("Proper Text", "\"Hello World!\"", true),
                Arguments.of("Escape Character: Tab", "\"1\\t2\"", true),
                Arguments.of("Escape Character: Backspace", "\"1\\b2\"", true),
                Arguments.of("Escape Character: Carriage Return", "\"1\\r2\"", true),
                Arguments.of("Escape Character: Double Quote", "\"1\\\"2\"", true),
                Arguments.of("Escape Character: Single Quote", "\"1\\'2\"", true),
                Arguments.of("Escape Character: New Line", "\"1\\n2\"", true),
                Arguments.of("Escape Character: Back Slash", "\"1\\\\2\"", true),
                Arguments.of("Spaces", "\"      \"", true),

                // Not Matching
                Arguments.of("Missing Quotes", "Hello", false),
                Arguments.of("Unterminated End", "\"This is an unterminated string.", false),
                Arguments.of("Unterminated Front", "This is an unterminated string.\"", false),
                Arguments.of("Unescaped Double Quote", "\"This is an unescaped \" quote string.\"", false),
                Arguments.of("Invalid Escape Character", "\"invald\\escape\"", false)

        );
    }

    /**
     * Asserts that the input matches the given pattern. This method doesn't do
     * much now, but you will see this concept in future assignments.
     */
    private static void test(String input, Pattern pattern, boolean success) {
        Assertions.assertEquals(success, pattern.matcher(input).matches());
    }

}
