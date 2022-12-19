use crate::lexer::*;
use crate::*;
use rssl_text::*;

fn preprocess_single_test(
    input: &str,
    source_manager: &mut SourceManager,
) -> Result<String, PreprocessError> {
    let tokens = preprocess_fragment(input, FileName("test.rssl".to_string()), source_manager)?;
    let string = unlex(&tokens, source_manager);
    Ok(string)
}

macro_rules! assert_text {
    ($left:expr, $right:expr) => {
        assert_text!(preprocess_single_test, $left, $right)
    };

    ($func:expr, $left:expr, $right:expr) => {
        let mut source_manager = SourceManager::new();
        let input: &str = $left;
        let preprocess_result: Result<String, PreprocessError> = $func(input, &mut source_manager);
        let reference: &str = $right;
        match preprocess_result {
            Ok(pt) => {
                assert_eq!(pt, reference);
            }
            Err(err) => {
                panic!("{}", err.display(&source_manager));
            }
        }
    };
}

macro_rules! assert_err {
    ($left:expr, $right:expr $(,)?) => {
        assert_err!(preprocess_single_test, $left, $right)
    };

    ($func:expr, $left:expr, $right:expr $(,)?) => {
        let mut source_manager = SourceManager::new();
        let input: &str = $left;
        let preprocess_result: Result<String, PreprocessError> = $func(input, &mut source_manager);
        assert_eq!(preprocess_result, Err($right));
    };
}

#[test]
fn test_empty() {
    assert_text!("", "");
    assert_text!("test", "test\n");
    assert_text!("t1\nt2", "t1\nt2\n");
    assert_text!("t1\r\nt2", "t1\r\nt2\n");
}

#[test]
fn test_define() {
    assert_text!("#define X 0\nX", "0\n");
    assert_text!("#define X 0\nX X", "0 0\n");
    assert_text!("#define X 1\r\nX", "1\n");
    assert_text!("#define X 2\n#define Y X\nX", "2\n");
    assert_text!("#define X 2\\\n + 3\nX", "2\n + 3\n");
    assert_text!("#define X", "");
    assert_text!("#define X 0\n#define Y 1\nX Y", "0 1\n");
    assert_text!("#define X 0\n#define XY 1\nXY X", "1 0\n");
    assert_text!("#define X (0)\nX", "(0)\n");
}

#[test]
fn test_macro_function() {
    // Single argument macro and invocation
    assert_text!("#define X(a) a\nX(2)", "2\n");

    // Double argument macro and invocation
    assert_text!("#define X(a,b) a+b\nX(2,3)", "2+3\n");

    // Macros can use their own name as one of their parameters - not that anyone should do this
    assert_text!("#define X(X,b) X+b\nX(2,3)", "2+3\n");

    // Test invoking a macro with values inside parenthesis
    assert_text!("#define X(a) a\nX((Y,Z))", "(Y,Z)\n");

    // Test multiple arguments with overlapping name substrings
    assert_text!(
        "#define X(a,ab,ba,b) a ab a ba b ab a\nX(0,1,2,3)",
        "0 1 0 2 3 1 0\n"
    );

    // Macro parameter lists should be comma separated identifiers
    assert_err!(
        "#define X(a b)\n",
        PreprocessError::InvalidDefine(SourceLocation::first().offset(8))
    );

    // Macro parameter lists should not be non-identifiers
    assert_err!(
        "#define X(&)\n",
        PreprocessError::InvalidDefine(SourceLocation::first().offset(8))
    );

    // Macro parameter lists should not have empty elements
    assert_err!(
        "#define X(,a)\n",
        PreprocessError::InvalidDefine(SourceLocation::first().offset(8))
    );
    assert_err!(
        "#define X( ,a)\n",
        PreprocessError::InvalidDefine(SourceLocation::first().offset(8))
    );
    assert_err!(
        "#define X(a,)\n",
        PreprocessError::InvalidDefine(SourceLocation::first().offset(8))
    );
    assert_err!(
        "#define X(a, )\n",
        PreprocessError::InvalidDefine(SourceLocation::first().offset(8))
    );
}

#[test]
fn test_macro_function_no_args() {
    assert_text!("#define X() A\nX()", "A\n");
    assert_text!("#define X()A\nX()", "A\n");
    assert_text!("#define X() A\nX", "X\n");
    assert_text!("#define X() A\nX X() X X()", "X A X A\n");
    assert_text!("#define X() A\nX ()", "A\n");
    assert_text!("#define X() A\nX ( )", "A\n");
    assert_text!("#define X( ) A\nX ( )", "A\n");

    assert_err!(
        "#define X(,) A\nX ( )",
        PreprocessError::InvalidDefine(SourceLocation::first().offset(8))
    );

    assert_err!(
        "#define  X(\t,\t) A\nX ( )",
        PreprocessError::InvalidDefine(SourceLocation::first().offset(9))
    );
}

#[test]
fn test_macro_function_line_continuation() {
    // Test multiple lines in function macro
    assert_text!("#define X(a,b) a+\\\nb\nX(2,3)", "2+\n3\n");
    assert_text!("#define X(a,b) a+\\\r\nb\nX(2,3)", "2+\r\n3\n");
    assert_text!("#define X(a,b)a+\\\nb\nX(2,3)", "2+\n3\n");
    assert_text!("#define X(a,b)\\\na+\\\nb\nX(2,3)", "2+\n3\n");
    assert_text!("#define X(a,b)\\\r\na+\\\nb\nX(2,3)", "2+\n3\n");
    assert_text!("#define X(\\\na,\\\nb) a+\\\nb\nX(2,3)", "2+\n3\n");
    assert_text!("#define\\\nX(a,b) a+\\\nb\nX(2,3)", "2+\n3\n");
}

#[test]
fn test_macro_function_in_define() {
    // Test invoking a macro with another define
    assert_text!("#define X(a) a\n#define Y 1\nX(Y)", "1\n");
}

#[test]
fn test_macro_function_chain() {
    // Base case for next text
    assert_text!(
        "#define Macro0(Arg0, Arg1) {Arg0,Arg1}\nMacro0(X, Y)",
        "{X,Y}\n"
    );

    // Test calling a macro in another macro
    assert_text!(
        "#define Macro0(Arg0, Arg1) {Arg0,Arg1}\n#define Macro1(Arg2, Arg3) Macro0(Arg2, Arg3)\nMacro1(X, Y)",
        "{X,Y}\n"
    );

    // Test calling a macro on the non-first line of another macro
    assert_text!(
        "#define Macro0(Arg0, Arg1) {Arg0,Arg1}\n#define Macro1(Arg2, Arg3) \\\nMacro0(Arg2, Arg3)\nMacro1(X, Y)",
        "{X,Y}\n"
    );

    // Test calling a macro from another macro with both macro args and other define args
    assert_text!(
        "#define Arg3 Y\n#define Macro0(Arg0, Arg1) {Arg0,Arg1}\n#define Macro1(Arg2) Macro0(Arg2, Arg3)\nMacro1(X)",
        "{X,Y}\n"
    );

    // Test invoking a macro with another macro invocation inside it
    assert_text!(
        "#define Macro0(Arg0) {0:Arg0}\n#define Macro1(Arg1) {1:Arg1}\nMacro0(Macro1(X))",
        "{0:{1:X}}\n"
    );

    // Test invoking a macro with another macro invocation inside it with multiple args
    assert_text!(
        "#define Macro0(Arg0, Arg1) {0:Arg0;Arg1}\n#define Macro1(Arg2, Arg3) {1:Arg2;Arg3}\nMacro0(Macro1(X,Y),Macro1(Z,W))",
        "{0:{1:X;Y};{1:Z;W}}\n"
    );
}

#[test]
fn test_macro_function_parameter_overrides_defines() {
    // Test invoking a macro with another define
    assert_text!("#define A 1\n#define B(A) A\nB(2)", "2\n");
}

#[test]
fn test_condition() {
    assert_err!("#if 0\nX", PreprocessError::ConditionChainNotFinished);
    assert_text!("#if 0\nX\n#endif", "");
    assert_text!("#if 1\nX\n#endif", "X\n");
    assert_text!("#if 0\nX\n#else\nY\n#endif", "Y\n");
    assert_text!("#if 1\nX\n#else\nY\n#endif", "X\n");
    assert_text!("#if !0\nX\n#else\nY\n#endif", "X\n");
    assert_text!("#if !1\nX\n#else\nY\n#endif", "Y\n");
    assert_text!("#if 0\n#if 0\nX\n#else\nY\n#endif\n#endif", "");
    assert_text!("#if 0\n#if 1\nX\n#else\nY\n#endif\n#endif", "");
    assert_text!("#if\t 1  \n X  \n #else \n Y \n#endif \n\t", " X  \n\t\n");
    assert_text!("#if\\\n 1  \n X  \n #else \n Y \n#endif \n\t", " X  \n\t\n");
    assert_text!("#define TRUE 1\n#if TRUE\nX\n#else\nY\n#endif", "X\n");
    assert_text!("#define TRUE\n#ifdef TRUE\nX\n#else\nY\n#endif", "X\n");
    assert_text!("#define TRUE\n#ifndef TRUE\nX\n#else\nY\n#endif", "Y\n");
    assert_text!("#define TRUE 1\n#ifdef TRUE\nX\n#else\nY\n#endif", "X\n");
    assert_text!("#define TRUE 0\n#ifndef TRUE\nX\n#else\nY\n#endif", "Y\n");
    assert_text!("#if 0\n#define X Y\n#endif\nX", "X\n");
    assert_text!("#if 1\n#define X Y\n#else\n#define X Z\n#endif\nX", "Y\n");
    assert_text!(
        "#if 1\n#define X Y\n#else\n#include\"fail\"\n#endif\nX",
        "Y\n"
    );
    assert_text!(
        "#if 1 // comment\n#define X Y\n#else // comment\n#include\"fail\"\n#endif // \
                   comment\nX",
        "Y\n"
    );
}

#[test]
fn test_condition_elif() {
    assert_err!(
        "#if 0\nX\n#elif 0\nY\n",
        PreprocessError::ConditionChainNotFinished
    );
    assert_text!("#if 0\nX\n#elif 0\nY\n#endif", "");
    assert_text!("#if 0\nX\n#elif 1\nY\n#endif", "Y\n");
    assert_text!("#if 1\nX\n#elif 0\nY\n#endif", "X\n");
    assert_text!("#if 1\nX\n#elif 1\nY\n#endif", "X\n");
    assert_text!("#if 0\nX\n#elif 0\nY\n#else\nZ\n#endif", "Z\n");
    assert_text!("#if 0\nX\n#elif 1\nY\n#else\nZ\n#endif", "Y\n");
    assert_text!("#if 1\nX\n#elif 0\nY\n#else\nZ\n#endif", "X\n");
    assert_text!("#if 0\n#if 0\nX\n#elif 0\nY\n#else\nZ\n#endif\n#endif", "");
    assert_text!("#if 0\n#if 0\nX\n#elif 1\nY\n#else\nZ\n#endif\n#endif", "");
    assert_text!("#if 0\n#if 1\nX\n#elif 0\nY\n#else\nZ\n#endif\n#endif", "");
}

#[test]
fn test_condition_defined() {
    assert_text!("#if defined(A)\nX\n#else\nY\n#endif", "Y\n");
    assert_text!("#if !defined(A)\nX\n#else\nY\n#endif", "X\n");
    assert_text!("#if defined A \nX\n#else\nY\n#endif", "Y\n");
    assert_text!("#if !defined A \nX\n#else\nY\n#endif", "X\n");
    assert_text!("#if defined A\nX\n#else\nY\n#endif", "Y\n");
    assert_text!("#if !defined A\nX\n#else\nY\n#endif", "X\n");
    assert_text!("#if defined A&&defined A\nX\n#else\nY\n#endif", "Y\n");
    assert_text!("#if !defined A&&!defined A\nX\n#else\nY\n#endif", "X\n");
    assert_text!("#if (defined A&&defined A)\nX\n#else\nY\n#endif", "Y\n");
    assert_text!("#if (!defined A&&!defined A)\nX\n#else\nY\n#endif", "X\n");
    assert_text!("#if (defined A)&&(defined A)\nX\n#else\nY\n#endif", "Y\n");
    assert_text!("#if (!defined A)&&(!defined A)\nX\n#else\nY\n#endif", "X\n");
    assert_text!("#if defined(\t\\\n\tA\t\\\n\t)\nX\n#else\nY\n#endif", "Y\n");
    assert_text!("#if defined\tA\nX\n#else\nY\n#endif", "Y\n");
    assert_text!("#if defined \\\nA\nX\n#else\nY\n#endif", "Y\n");
    assert_text!("#if defined\\\nA\nX\n#else\nY\n#endif", "Y\n");
    assert_text!("#if defined/**/A\nX\n#else\nY\n#endif", "Y\n");
    assert_text!("#define A 1\n#if defined(A)\nX\n#else\nY\n#endif", "X\n");
    assert_text!("#define A 1\n#if !defined(A)\nX\n#else\nY\n#endif", "Y\n");
    assert_text!("#define A 0\n#if defined(A)\nX\n#else\nY\n#endif", "X\n");
    assert_text!("#define A 0\n#if !defined(A)\nX\n#else\nY\n#endif", "Y\n");
    assert_text!(
        "#if defined(A)\ndefined(B)\n#else\ndefined(C)\n#endif",
        "defined(C)\n"
    );

    // defined is not evaluated in other contexts
    assert_text!("defined(A)", "defined(A)\n");
    assert_text!("!defined(A)", "!defined(A)\n");
}

#[test]
fn test_include() {
    fn pf(contents: &str, source_manager: &mut SourceManager) -> Result<String, PreprocessError> {
        let mut files = [
            ("test.rssl", contents),
            ("1.csh", "X"),
            ("2.csh", "Y"),
            ("p1.rssl", "#pragma once\n1"),
            ("p2.rssl", "2"),
        ];
        let tokens = preprocess("test.rssl", source_manager, &mut files)?;
        let string = unlex(&tokens, source_manager);
        Ok(string)
    }

    // Unknown files should always fail
    assert_err!(
        pf,
        "#include \"unknown.csh\"",
        PreprocessError::FailedToFindFile(
            SourceLocation::first().offset(1),
            "unknown.csh".to_string(),
            IncludeError::FileNotFound
        )
    );
    assert_err!(
        pf,
        "#include",
        PreprocessError::InvalidInclude(SourceLocation::first().offset(1))
    );
    assert_err!(
        pf,
        "#include\n",
        PreprocessError::InvalidInclude(SourceLocation::first().offset(1))
    );
    // Normal case
    assert_text!(pf, "#include \"1.csh\"\n", "X\n");
    // End of file include
    assert_text!(pf, "#include \"1.csh\"", "X\n");
    // Extra whitespace
    assert_text!(pf, "#include \"1.csh\"\t\n", "X\n");
    // Less whitespace
    assert_text!(pf, "#include\"1.csh\"\n", "X\n");
    // Alternative delimiters (not treated differently currently)
    assert_text!(pf, "#include <1.csh>\n", "X\n");
    assert_text!(pf, "#include<1.csh>\n", "X\n");
    assert_err!(
        pf,
        "#include \"1.csh>\n",
        PreprocessError::LexerError(LexerError::new(
            LexerErrorReason::Unknown,
            SourceLocation::first().offset(9),
        ))
    );
    assert_err!(
        pf,
        "#include <1.csh\"\n",
        PreprocessError::LexerError(LexerError::new(
            LexerErrorReason::Unknown,
            SourceLocation::first().offset(15),
        ))
    );
    // Comments after includes needs to work
    assert_text!(pf, "#include \"1.csh\" // include \n", "X\n");
    assert_text!(pf, "#include \"1.csh\"\n#include \"2.csh\"", "X\nY\n");
    // We don't want to read files that are #if'd out
    assert_text!(
        pf,
        "#if 1\n#include \"1.csh\"\n#else\n#include \"unknown.csh\"\n#endif",
        "X\n"
    );
    assert_text!(
        pf,
        "#if 0\n#include \"unknown.csh\"\n#else\n#include \"2.csh\"\n#endif",
        "Y\n"
    );

    // Check #pragma once
    assert_text!(
        pf,
        "#include \"p1.rssl\"\n#include \"p1.rssl\"\n#include \"p2.rssl\"\n#include \"p2.rssl\"\n",
        "1\n2\n2\n"
    );
}

#[test]
fn test_concat() {
    // Nothing occurs when not in a macro expanding context
    assert_text!("a ## b", "a ## b\n");

    // ## should be allowed inside a #define
    assert_text!("#define M a ## b\n", "");

    // ## is not allowed in macro arguments
    assert_err!(
        "#define M(a ## b)\n",
        PreprocessError::InvalidDefine(SourceLocation::first().offset(8))
    );

    // ## in a non-function macro can combine tokens
    assert_text!("#define M a ## b\nM", "ab\n");

    // ## in a function macro can combine tokens from the arguments
    assert_text!("#define M(a, b) a ## b\nM(c, d)", "cd\n");

    // ## in a function macro applies to the individual tokens in the arguments
    assert_text!("#define M(a, b) a ## b\nM(c d, e f)", "c de f\n");

    // Ensure it still works with lots of pointless whitespace
    // May want to review how much of the whitespace we want in the output
    // currently the \n before the c also makes it
    assert_text!(
        "#define M(a, b) a\t\\\n##\t\\\nb\nM(\t\\\n\nc,\t\\\n\nd)",
        "\ncd\n"
    );

    // ## in the arguments should not apply
    assert_text!("#define M(a, b, c) a b c\nM(0, ##, 1)", "0 ## 1\n");

    // ## in recursive macro calls only applies at the end
    assert_text!(
        "#define M1(a, b) a ## b\n#define M2(a, b) M1(a, b)\nM2(c, d)",
        "cd\n"
    );
}
