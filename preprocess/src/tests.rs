use crate::*;
use rssl_text::*;

fn preprocess_single_test(input: &str) -> Result<PreprocessedText, PreprocessError> {
    preprocess_single(input, FileName("test.rssl".to_string()))
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
    assert_text!(pp("#define X(a) a\nX(2)"), "2");
    assert_text!(pp("#define X(a,b) a+b\nX(2,3)"), "2+3");
    assert_text!(pp("#define X(X,b) X+b\nX(2,3)"), "2+3");
    assert_text!(pp("#define X(a,b) a+\\\nb\nX(2,3)"), "2+\n3");
    assert_text!(pp("#define X(a,b) a+\\\r\nb\nX(2,3)"), "2+\r\n3");
    assert_text!(pp("#define X"), "");
    assert_text!(pp("#define X 0\n#define Y 1\nX Y"), "0 1");
    assert_text!(pp("#define X 0\n#define XY 1\nXY X"), "1 0");
    assert_text!(pp("#define X(a) a\n#define Y 1\nX(Y)"), "1");
    assert_text!(
        pp("#define X(a,ab,ba,b) a ab a ba b ab a\nX(0,1,2,3)"),
        "0 1 0 2 3 1 0"
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
fn test_include() {
    struct TestFileLoader;
    impl IncludeHandler for TestFileLoader {
        fn load(&mut self, file_name: &str) -> Result<String, IncludeError> {
            Ok(match file_name.as_ref() {
                "1.csh" => "X",
                "2.csh" => "Y",
                _ => return Err(IncludeError::FileNotFound),
            }
            .to_string())
        }
    }

    fn pf(contents: &str) -> Result<PreprocessedText, PreprocessError> {
        preprocess(
            contents,
            FileName("test.rssl".to_string()),
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
}
