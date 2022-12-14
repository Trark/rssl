use crate::*;
use rssl_text::*;

fn preprocess_single_test(input: &str) -> Result<PreprocessedText, PreprocessError> {
    let mut source_manager = SourceManager::new();
    preprocess_fragment(
        input,
        FileName("test.rssl".to_string()),
        &mut source_manager,
    )
}

macro_rules! assert_text {
    ($left:expr, $right:expr $(,)?) => {
        let preprocess_result: Result<PreprocessedText, PreprocessError> = $left;
        match preprocess_result {
            Ok(pt) => {
                assert_eq!(pt.as_str(), $right);
            }
            Err(err) => {
                panic!("{}", err);
            }
        }
    };
}

macro_rules! assert_err {
    ($left:expr, $right:expr $(,)?) => {
        let preprocess_result: Result<PreprocessedText, PreprocessError> = $left;
        assert_eq!(preprocess_result, Err($right));
    };
}

#[test]
fn test_empty() {
    let pp = preprocess_single_test;
    assert_text!(pp(""), "");
    assert_text!(pp("test"), "test");
    assert_text!(pp("t1\nt2"), "t1\nt2");
    assert_text!(pp("t1\r\nt2"), "t1\r\nt2");
}

#[test]
fn test_define() {
    let pp = preprocess_single_test;
    assert_text!(pp("#define X 0\nX"), "0");
    assert_text!(pp("#define X 0\nX X"), "0 0");
    assert_text!(pp("#define X 1\r\nX"), "1");
    assert_text!(pp("#define X 2\n#define Y X\nX"), "2");
    assert_text!(pp("#define X 2\\\n + 3\nX"), "2\n + 3");
    assert_text!(pp("#define X"), "");
    assert_text!(pp("#define X 0\n#define Y 1\nX Y"), "0 1");
    assert_text!(pp("#define X 0\n#define XY 1\nXY X"), "1 0");
}

#[test]
fn test_define_function() {
    let pp = preprocess_single_test;
    assert_text!(pp("#define X(a) a\nX(2)"), "2");
    assert_text!(pp("#define X(a,b) a+b\nX(2,3)"), "2+3");
    assert_text!(pp("#define X(X,b) X+b\nX(2,3)"), "2+3");

    // Test multiple lines in function macro
    // The first line currently must have a space after the right parenthesis
    // It can not immediately go into the new line
    assert_text!(pp("#define X(a,b) a+\\\nb\nX(2,3)"), "2+\n3");
    assert_text!(pp("#define X(a,b) a+\\\r\nb\nX(2,3)"), "2+\r\n3");

    // Test invoking a macro with another define
    assert_text!(pp("#define X(a) a\n#define Y 1\nX(Y)"), "1");

    // Test multiple arguments with overlapping name substrings
    assert_text!(
        pp("#define X(a,ab,ba,b) a ab a ba b ab a\nX(0,1,2,3)"),
        "0 1 0 2 3 1 0"
    );

    // Base case for next text
    assert_text!(
        pp("#define Macro0(Arg0, Arg1) {Arg0,Arg1}\nMacro0(X, Y)"),
        "{X,Y}"
    );

    // Test calling a macro in another macro
    assert_text!(
        pp("#define Macro0(Arg0, Arg1) {Arg0,Arg1}\n#define Macro1(Arg2, Arg3) Macro0(Arg2, Arg3)\nMacro1(X, Y)"),
        "{X,Y}"
    );

    // Test calling a macro on the non-first line of another macro
    assert_text!(
        pp("#define Macro0(Arg0, Arg1) {Arg0,Arg1}\n#define Macro1(Arg2, Arg3) \\\nMacro0(Arg2, Arg3)\nMacro1(X, Y)"),
        "\n{X,Y}"
    );

    // Test calling a macro from another macro with both macro args and other define args
    assert_text!(
        pp("#define Arg3 Y\n#define Macro0(Arg0, Arg1) {Arg0,Arg1}\n#define Macro1(Arg2) Macro0(Arg2, Arg3)\nMacro1(X)"),
        "{X,Y}"
    );

    // Test invoking a macro with another macro invocation inside it
    assert_text!(
        pp("#define Macro0(Arg0) {0:Arg0}\n#define Macro1(Arg1) {1:Arg1}\nMacro0(Macro1(X))"),
        "{0:{1:X}}"
    );
}

#[test]
fn test_condition() {
    let pp = preprocess_single_test;
    assert_err!(pp("#if 0\nX"), PreprocessError::ConditionChainNotFinished);
    assert_text!(pp("#if 0\nX\n#endif"), "");
    assert_text!(pp("#if 1\nX\n#endif"), "X\n");
    assert_text!(pp("#if 0\nX\n#else\nY\n#endif"), "Y\n");
    assert_text!(pp("#if 1\nX\n#else\nY\n#endif"), "X\n");
    assert_text!(pp("#if !0\nX\n#else\nY\n#endif"), "X\n");
    assert_text!(pp("#if !1\nX\n#else\nY\n#endif"), "Y\n");
    assert_text!(pp("#if 0\n#if 0\nX\n#else\nY\n#endif\n#endif"), "");
    assert_text!(pp("#if 0\n#if 1\nX\n#else\nY\n#endif\n#endif"), "");
    assert_text!(pp("#if\t 1  \n X  \n #else \n Y \n#endif \n\t"), " X  \n\t");
    assert_text!(pp("#define TRUE 1\n#if TRUE\nX\n#else\nY\n#endif"), "X\n");
    assert_text!(pp("#define TRUE\n#ifdef TRUE\nX\n#else\nY\n#endif"), "X\n");
    assert_text!(pp("#define TRUE\n#ifndef TRUE\nX\n#else\nY\n#endif"), "Y\n");
    assert_text!(
        pp("#define TRUE 1\n#ifdef TRUE\nX\n#else\nY\n#endif"),
        "X\n"
    );
    assert_text!(
        pp("#define TRUE 0\n#ifndef TRUE\nX\n#else\nY\n#endif"),
        "Y\n"
    );
    assert_text!(pp("#if 0\n#define X Y\n#endif\nX"), "X");
    assert_text!(pp("#if 1\n#define X Y\n#else\n#define X Z\n#endif\nX"), "Y");
    assert_text!(
        pp("#if 1\n#define X Y\n#else\n#include\"fail\"\n#endif\nX"),
        "Y"
    );
    assert_text!(
        pp(
            "#if 1 // comment\n#define X Y\n#else // comment\n#include\"fail\"\n#endif // \
                   comment\nX"
        ),
        "Y"
    );
}

#[test]
fn test_condition_elif() {
    let pp = preprocess_single_test;
    assert_err!(
        pp("#if 0\nX\n#elif 0\nY\n"),
        PreprocessError::ConditionChainNotFinished
    );
    assert_text!(pp("#if 0\nX\n#elif 0\nY\n#endif"), "");
    assert_text!(pp("#if 0\nX\n#elif 1\nY\n#endif"), "Y\n");
    assert_text!(pp("#if 1\nX\n#elif 0\nY\n#endif"), "X\n");
    assert_text!(pp("#if 1\nX\n#elif 1\nY\n#endif"), "X\n");
    assert_text!(pp("#if 0\nX\n#elif 0\nY\n#else\nZ\n#endif"), "Z\n");
    assert_text!(pp("#if 0\nX\n#elif 1\nY\n#else\nZ\n#endif"), "Y\n");
    assert_text!(pp("#if 1\nX\n#elif 0\nY\n#else\nZ\n#endif"), "X\n");
    assert_text!(
        pp("#if 0\n#if 0\nX\n#elif 0\nY\n#else\nZ\n#endif\n#endif"),
        ""
    );
    assert_text!(
        pp("#if 0\n#if 0\nX\n#elif 1\nY\n#else\nZ\n#endif\n#endif"),
        ""
    );
    assert_text!(
        pp("#if 0\n#if 1\nX\n#elif 0\nY\n#else\nZ\n#endif\n#endif"),
        ""
    );
}

#[test]
fn test_condition_defined() {
    let pp = preprocess_single_test;
    assert_text!(pp("#if defined(A)\nX\n#else\nY\n#endif"), "Y\n");
    assert_text!(pp("#if !defined(A)\nX\n#else\nY\n#endif"), "X\n");
    assert_text!(pp("#if defined A \nX\n#else\nY\n#endif"), "Y\n");
    assert_text!(pp("#if !defined A \nX\n#else\nY\n#endif"), "X\n");
    assert_text!(pp("#if defined A\nX\n#else\nY\n#endif"), "Y\n");
    assert_text!(pp("#if !defined A\nX\n#else\nY\n#endif"), "X\n");
    assert_text!(pp("#if defined A&&defined A\nX\n#else\nY\n#endif"), "Y\n");
    assert_text!(pp("#if !defined A&&!defined A\nX\n#else\nY\n#endif"), "X\n");
    assert_text!(
        pp("#define A 1\n#if defined(A)\nX\n#else\nY\n#endif"),
        "X\n"
    );
    assert_text!(
        pp("#define A 1\n#if !defined(A)\nX\n#else\nY\n#endif"),
        "Y\n"
    );
    assert_text!(
        pp("#define A 0\n#if defined(A)\nX\n#else\nY\n#endif"),
        "X\n"
    );
    assert_text!(
        pp("#define A 0\n#if !defined(A)\nX\n#else\nY\n#endif"),
        "Y\n"
    );
    assert_text!(pp("defined(A)"), "defined(A)");
    assert_text!(pp("!defined(A)"), "!defined(A)");
    assert_text!(
        pp("#if defined(A)\ndefined(B)\n#else\ndefined(C)\n#endif"),
        "defined(C)\n"
    );
}

#[test]
fn test_include() {
    struct TestFileLoader;
    impl IncludeHandler for TestFileLoader {
        fn load(&mut self, file_name: &str) -> Result<FileData, IncludeError> {
            Ok(FileData {
                real_name: format!("./test/{}", file_name.to_string()),
                contents: match file_name.as_ref() {
                    "1.csh" => "X",
                    "2.csh" => "Y",
                    "p1.rssl" => "#pragma once\n1",
                    "p2.rssl" => "2",
                    _ => return Err(IncludeError::FileNotFound),
                }
                .to_string(),
            })
        }
    }

    fn pf(contents: &str) -> Result<PreprocessedText, PreprocessError> {
        let mut source_manager = SourceManager::new();
        preprocess_direct(
            contents,
            FileName("test.rssl".to_string()),
            &mut source_manager,
            &mut TestFileLoader,
        )
    }

    // Unknown files should always fail
    assert_err!(
        pf("#include \"unknown.csh\""),
        PreprocessError::FailedToFindFile("unknown.csh".to_string(), IncludeError::FileNotFound)
    );
    assert_err!(pf("#include"), PreprocessError::InvalidInclude);
    assert_err!(pf("#include\n"), PreprocessError::InvalidInclude);
    // Normal case
    assert_text!(pf("#include \"1.csh\"\n"), "X\n");
    // End of file include
    assert_text!(pf("#include \"1.csh\""), "X");
    // Extra whitespace
    assert_text!(pf("#include \"1.csh\"\t\n"), "X\n");
    // Less whitespace
    assert_text!(pf("#include\"1.csh\"\n"), "X\n");
    // Alternative delimiters (not treated differently currently)
    assert_text!(pf("#include <1.csh>\n"), "X\n");
    assert_text!(pf("#include<1.csh>\n"), "X\n");
    assert_err!(pf("#include \"1.csh>\n"), PreprocessError::InvalidInclude);
    assert_err!(pf("#include <1.csh\"\n"), PreprocessError::InvalidInclude);
    // Comments after includes needs to work
    assert_text!(pf("#include \"1.csh\" // include \n"), "X\n");
    assert_text!(pf("#include \"1.csh\"\n#include \"2.csh\""), "X\nY");
    // We don't want to read files that are #if'd out
    assert_text!(
        pf("#if 1\n#include \"1.csh\"\n#else\n#include \"unknown.csh\"\n#endif"),
        "X\n"
    );
    assert_text!(
        pf("#if 0\n#include \"unknown.csh\"\n#else\n#include \"2.csh\"\n#endif"),
        "Y\n"
    );

    // Check #pragma once
    assert_text!(
        pf("#include \"p1.rssl\"\n#include \"p1.rssl\"\n#include \"p2.rssl\"\n#include \"p2.rssl\"\n"),
        "1\n\n2\n2\n"
    );
}
