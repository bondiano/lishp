use crate::value::{BinaryOperator, BinaryPredicate, LishpValue, SpecialForm};
use nom::{
  Err, IResult, Parser,
  branch::alt,
  bytes::complete::{tag, take_while, take_while1},
  character::complete::{char, digit1, multispace1, one_of},
  combinator::{map, map_res, opt, recognize, value},
  multi::many0,
  sequence::{delimited, preceded},
};
use std::rc::Rc;

#[inline]
fn is_delimiter(c: char) -> bool {
  c.is_whitespace() || c == ')' || c == '(' || c == ';' || c == ','
}

#[inline]
fn with_delimiter_check<'a, O, F>(mut parser: F) -> impl FnMut(&'a str) -> IResult<&'a str, O>
where
  F: Parser<&'a str, Output = O, Error = nom::error::Error<&'a str>>,
{
  move |i: &'a str| {
    let (rest, value) = parser.parse(i)?;
    if let Some(next_char) = rest.chars().next()
      && !is_delimiter(next_char)
    {
      return Err(nom::Err::Failure(nom::error::Error::new(
        i,
        nom::error::ErrorKind::Digit,
      )));
    }
    Ok((rest, value))
  }
}

#[inline]
fn parse_exponent(i: &str) -> IResult<&str, &str> {
  recognize((one_of("eE"), opt(one_of("+-")), digit1)).parse(i)
}

#[inline]
fn parse_scientific(i: &str) -> IResult<&str, LishpValue> {
  with_delimiter_check(map_res(
    recognize((
      opt(char('-')),
      alt((
        recognize((digit1, char('.'), opt(digit1))),
        recognize((char('.'), digit1)),
        digit1,
      )),
      parse_exponent,
    )),
    |s: &str| s.parse::<f64>().map(LishpValue::Double),
  ))(i)
}

#[inline]
fn parse_float(i: &str) -> IResult<&str, LishpValue> {
  with_delimiter_check(map_res(
    alt((
      recognize((char('.'), digit1)),
      recognize((opt(char('-')), digit1, char('.'), opt(digit1))),
      recognize((char('-'), char('.'), digit1)),
    )),
    |s: &str| s.parse::<f64>().map(LishpValue::Double),
  ))(i)
}

#[inline]
fn parse_integer(i: &str) -> IResult<&str, LishpValue> {
  with_delimiter_check(map_res(recognize((opt(char('-')), digit1)), |s: &str| {
    s.parse::<i64>().map(LishpValue::Integer)
  }))(i)
}

#[inline]
fn parse_number(i: &str) -> IResult<&str, LishpValue> {
  alt((parse_scientific, parse_float, parse_integer)).parse(i)
}

#[inline]
fn is_symbol_char(c: char) -> bool {
  c.is_alphanumeric()
    || c == '-'
    || c == '_'
    || c == '+'
    || c == '*'
    || c == '/'
    || c == '='
    || c == '<'
    || c == '>'
    || c == '?'
    || c == '!'
}

#[inline]
fn parse_keyword(keyword: &'static str) -> impl Fn(&str) -> IResult<&str, &str> {
  move |i: &str| {
    let (rest, matched) = tag(keyword)(i)?;
    if let Some(next_char) = rest.chars().next()
      && is_symbol_char(next_char)
    {
      return Err(nom::Err::Error(nom::error::Error::new(
        i,
        nom::error::ErrorKind::Tag,
      )));
    }
    Ok((rest, matched))
  }
}

fn parse_special_form(i: &str) -> IResult<&str, LishpValue> {
  alt((
    value(
      LishpValue::SpecialForm(SpecialForm::Define),
      parse_keyword("def"),
    ),
    value(
      LishpValue::SpecialForm(SpecialForm::Set),
      parse_keyword("set!"),
    ),
    value(
      LishpValue::SpecialForm(SpecialForm::If),
      parse_keyword("if"),
    ),
    value(
      LishpValue::SpecialForm(SpecialForm::Quote),
      parse_keyword("quote"),
    ),
    value(
      LishpValue::SpecialForm(SpecialForm::Eval),
      parse_keyword("eval"),
    ),
    value(
      LishpValue::SpecialForm(SpecialForm::Car),
      parse_keyword("car"),
    ),
    value(
      LishpValue::SpecialForm(SpecialForm::Cdr),
      parse_keyword("cdr"),
    ),
    value(
      LishpValue::SpecialForm(SpecialForm::Cons),
      parse_keyword("cons"),
    ),
    value(
      LishpValue::SpecialForm(SpecialForm::Do),
      parse_keyword("do"),
    ),
    value(
      LishpValue::SpecialForm(SpecialForm::TypeOf),
      parse_keyword("typeof"),
    ),
    value(
      LishpValue::SpecialForm(SpecialForm::Read),
      parse_keyword("read"),
    ),
    value(
      LishpValue::SpecialForm(SpecialForm::Print),
      parse_keyword("print"),
    ),
    value(
      LishpValue::SpecialForm(SpecialForm::Symbol),
      parse_keyword("symbol"),
    ),
    value(
      LishpValue::SpecialForm(SpecialForm::Load),
      parse_keyword("load"),
    ),
    value(
      LishpValue::SpecialForm(SpecialForm::Lambda),
      parse_keyword("lambda"),
    ),
    value(
      LishpValue::SpecialForm(SpecialForm::Dambda),
      parse_keyword("dambda"),
    ),
  ))
  .parse(i)
}

fn parse_binary_operator(i: &str) -> IResult<&str, LishpValue> {
  alt((
    value(
      LishpValue::BinaryOperator(BinaryOperator::Add),
      parse_keyword("_+_"),
    ),
    value(
      LishpValue::BinaryOperator(BinaryOperator::Subtract),
      parse_keyword("_-_"),
    ),
    value(
      LishpValue::BinaryOperator(BinaryOperator::Multiply),
      parse_keyword("_*_"),
    ),
    value(
      LishpValue::BinaryOperator(BinaryOperator::Divide),
      parse_keyword("_/_"),
    ),
    value(
      LishpValue::BinaryOperator(BinaryOperator::Modulo),
      parse_keyword("_%_"),
    ),
    value(
      LishpValue::BinaryOperator(BinaryOperator::StrConcat),
      parse_keyword("_++_"),
    ),
  ))
  .parse(i)
}

fn parse_binary_predicate(i: &str) -> IResult<&str, LishpValue> {
  alt((
    value(
      LishpValue::BinaryPredicate(BinaryPredicate::Equals),
      parse_keyword("_=_"),
    ),
    value(
      LishpValue::BinaryPredicate(BinaryPredicate::LessThan),
      parse_keyword("_<_"),
    ),
    value(
      LishpValue::BinaryPredicate(BinaryPredicate::GreaterThan),
      parse_keyword("_>_"),
    ),
  ))
  .parse(i)
}

fn parse_string_content(i: &str) -> IResult<&str, String> {
  let mut result = String::with_capacity(i.len().min(256));
  let mut input = i;

  loop {
    let special_pos = input.find(['\\', '"']).unwrap_or(input.len());

    if special_pos > 0 {
      result.push_str(&input[..special_pos]);
      input = &input[special_pos..];
    }

    if input.is_empty() {
      return Err(nom::Err::Error(nom::error::Error::new(
        input,
        nom::error::ErrorKind::Eof,
      )));
    }

    match input.chars().next() {
      Some('"') => break,
      Some('\\') => {
        input = &input[1..];
        if let Some(c) = input.chars().next() {
          let ch = match c {
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            '\\' => '\\',
            '"' => '"',
            _ => c,
          };
          result.push(ch);
          input = &input[c.len_utf8()..];
        } else {
          return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Eof,
          )));
        }
      }
      _ => {
        return Err(nom::Err::Error(nom::error::Error::new(
          input,
          nom::error::ErrorKind::Char,
        )));
      }
    }
  }

  Ok((input, result))
}

fn parse_string(i: &str) -> IResult<&str, LishpValue> {
  map(
    delimited(char('"'), parse_string_content, char('"')),
    |s: String| LishpValue::String(s.into()),
  )
  .parse(i)
}

fn parse_bool(i: &str) -> IResult<&str, LishpValue> {
  alt((
    map(parse_keyword("true"), |_| LishpValue::Bool(true)),
    map(parse_keyword("false"), |_| LishpValue::Bool(false)),
  ))
  .parse(i)
}

fn parse_nil(i: &str) -> IResult<&str, LishpValue> {
  map(parse_keyword("nil"), |_| LishpValue::Nil).parse(i)
}

fn parse_symbol(i: &str) -> IResult<&str, LishpValue> {
  if let Some(first_char) = i.chars().next()
    && first_char.is_ascii_digit()
  {
    return Err(nom::Err::Error(nom::error::Error::new(
      i,
      nom::error::ErrorKind::Alpha,
    )));
  }

  map(take_while1(is_symbol_char), |s: &str| LishpValue::Symbol {
    name: s.into(),
  })
  .parse(i)
}

#[inline]
fn parse_comment(i: &str) -> IResult<&str, ()> {
  value((), preceded(char(';'), take_while(|c| c != '\n'))).parse(i)
}

#[inline]
fn skip_ws_and_comments(i: &str) -> IResult<&str, ()> {
  value(
    (),
    many0(alt((
      value((), multispace1),
      parse_comment,
      value((), char(',')),
    ))),
  )
  .parse(i)
}

fn parse_quoted(i: &str) -> IResult<&str, LishpValue> {
  let (rest, _) = char('\'')(i)?;

  let (after_ws, _) = skip_ws_and_comments(rest)?;
  if after_ws.is_empty() {
    return Err(nom::Err::Failure(nom::error::Error::new(
      i,
      nom::error::ErrorKind::Eof,
    )));
  }

  if let Some(')') = after_ws.chars().next() {
    return Err(nom::Err::Failure(nom::error::Error::new(
      i,
      nom::error::ErrorKind::Tag,
    )));
  }

  let (rest, expr) = parse_value(rest)?;

  Ok((
    rest,
    LishpValue::Cons(
      Rc::new(LishpValue::SpecialForm(SpecialForm::Quote)),
      Rc::new(LishpValue::Cons(Rc::new(expr), Rc::new(LishpValue::Nil))),
    ),
  ))
}

fn parse_value(i: &str) -> IResult<&str, LishpValue> {
  preceded(
    skip_ws_and_comments,
    alt((
      parse_quoted,
      parse_nil,
      parse_bool,
      parse_number,
      parse_string,
      parse_special_form,
      parse_binary_operator,
      parse_binary_predicate,
      parse_list,
      parse_symbol,
    )),
  )
  .parse(i)
}

fn parse_list(i: &str) -> IResult<&str, LishpValue> {
  map(
    delimited(
      char('('),
      many0(preceded(skip_ws_and_comments, parse_value)),
      preceded(skip_ws_and_comments, char(')')),
    ),
    crate::value::list,
  )
  .parse(i)
}

#[derive(Debug, PartialEq)]
pub enum ParseError {
  Incomplete,
  UnmatchedClosing,
  Error(String),
}

enum BalanceState {
  Balanced,
  Incomplete,
  UnmatchedClosing,
}

fn check_balanced(input: &str) -> BalanceState {
  let mut paren_depth = 0;
  let mut in_string = false;
  let mut escaped = false;

  for c in input.chars() {
    if in_string {
      if escaped {
        escaped = false;
      } else if c == '\\' {
        escaped = true;
      } else if c == '"' {
        in_string = false;
      }
    } else {
      match c {
        '"' => in_string = true,
        '(' => paren_depth += 1,
        ')' => {
          paren_depth -= 1;
          if paren_depth < 0 {
            return BalanceState::UnmatchedClosing;
          }
        }
        _ => {}
      }
    }
  }

  if in_string || paren_depth > 0 {
    BalanceState::Incomplete
  } else {
    BalanceState::Balanced
  }
}

pub fn parse(input: &str) -> Result<Option<(LishpValue, &str)>, ParseError> {
  let trimmed = match skip_ws_and_comments(input) {
    Ok((rest, _)) => rest,
    Err(_) => input,
  };

  if trimmed.is_empty() {
    return Ok(None);
  }

  match check_balanced(trimmed) {
    BalanceState::Incomplete => return Err(ParseError::Incomplete),
    BalanceState::UnmatchedClosing => return Err(ParseError::UnmatchedClosing),
    BalanceState::Balanced => {}
  }

  match parse_value(trimmed) {
    Ok((remaining, value)) => Ok(Some((value, remaining))),
    Err(Err::Error(_)) | Err(Err::Failure(_)) => {
      let has_quote_error = trimmed.starts_with('\'')
        || trimmed.contains("')")
        || trimmed.chars().any(|c| c == '\'') && trimmed.chars().filter(|c| *c == ')').count() > 0;

      let error_msg = if has_quote_error {
        "Expected valid expression after quote ('), but found nothing or invalid syntax"
      } else {
        "Parse error: invalid syntax"
      };

      Err(ParseError::Error(format!(
        "{} at position: {}",
        error_msg,
        trimmed.get(..20).unwrap_or(trimmed)
      )))
    }
    Err(Err::Incomplete(_)) => Err(ParseError::Incomplete),
  }
}

pub fn parse_repl(input: &str) -> Result<Option<(LishpValue, &str)>, ParseError> {
  let trimmed = match skip_ws_and_comments(input) {
    Ok((rest, _)) => rest,
    Err(_) => input,
  };

  if trimmed.is_empty() {
    return Ok(None);
  }

  match check_balanced(trimmed) {
    BalanceState::Incomplete => return Err(ParseError::Incomplete),
    BalanceState::UnmatchedClosing => return Err(ParseError::UnmatchedClosing),
    BalanceState::Balanced => {}
  }

  match parse_value(trimmed) {
    Ok((remaining, value)) => Ok(Some((value, remaining))),
    Err(Err::Error(_)) | Err(Err::Failure(_)) => {
      let has_quote_error = trimmed.starts_with('\'')
        || trimmed.contains("')")
        || trimmed.chars().any(|c| c == '\'') && trimmed.chars().filter(|c| *c == ')').count() > 0;

      let error_msg = if has_quote_error {
        "Expected valid expression after quote ('), but found nothing or invalid syntax"
      } else {
        "Parse error: invalid syntax"
      };

      Err(ParseError::Error(format!(
        "{} at position: {}",
        error_msg,
        trimmed.get(..20).unwrap_or(trimmed)
      )))
    }
    Err(Err::Incomplete(_)) => Err(ParseError::Incomplete),
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::{lishp_list, sym, value::cons};

  #[test]
  fn test_parse_integer() {
    assert_eq!(
      parse("42").expect("Failed to parse integer 42"),
      Some((LishpValue::Integer(42), ""))
    );
    assert_eq!(
      parse("-42").expect("Failed to parse integer -42"),
      Some((LishpValue::Integer(-42), ""))
    );
    assert_eq!(
      parse("0").expect("Failed to parse integer 0"),
      Some((LishpValue::Integer(0), ""))
    );
  }

  #[test]
  fn test_parse_float() {
    assert_eq!(
      parse("42.5").expect("Failed to parse float 42.5"),
      Some((LishpValue::Double(42.5), ""))
    );
    assert_eq!(
      parse("-42.5").expect("Failed to parse float -42.5"),
      Some((LishpValue::Double(-42.5), ""))
    );
    assert_eq!(
      parse("0.5").expect("Failed to parse float 0.5"),
      Some((LishpValue::Double(0.5), ""))
    );
    assert_eq!(
      parse(".5").expect("Failed to parse float .5"),
      Some((LishpValue::Double(0.5), ""))
    );
    assert_eq!(
      parse("-.5").expect("Failed to parse float -.5"),
      Some((LishpValue::Double(-0.5), ""))
    );
    assert_eq!(
      parse("42.").expect("Failed to parse float 42."),
      Some((LishpValue::Double(42.0), ""))
    );
  }

  #[test]
  fn test_parse_scientific() {
    assert_eq!(
      parse("1e10").expect("Failed to parse scientific 1e10"),
      Some((LishpValue::Double(1e10), ""))
    );
    assert_eq!(
      parse("1E10").expect("Failed to parse scientific 1E10"),
      Some((LishpValue::Double(1e10), ""))
    );
    assert_eq!(
      parse("1.5e10").expect("Failed to parse scientific 1.5e10"),
      Some((LishpValue::Double(1.5e10), ""))
    );
    assert_eq!(
      parse("1e-10").expect("Failed to parse scientific 1e-10"),
      Some((LishpValue::Double(1e-10), ""))
    );
    assert_eq!(
      parse("1.5e-10").expect("Failed to parse scientific 1.5e-10"),
      Some((LishpValue::Double(1.5e-10), ""))
    );
    assert_eq!(
      parse("-1e10").expect("Failed to parse scientific -1e10"),
      Some((LishpValue::Double(-1e10), ""))
    );
    assert_eq!(
      parse("1e+10").expect("Failed to parse scientific 1e+10"),
      Some((LishpValue::Double(1e10), ""))
    );
  }

  #[test]
  fn test_parse_string() {
    assert_eq!(
      parse("\"hello\"").expect("Failed to parse string \"hello\""),
      Some((LishpValue::String("hello".into()), ""))
    );
    assert_eq!(
      parse("\"hello\\nworld\"").expect("Failed to parse string with escaped newline"),
      Some((LishpValue::String("hello\nworld".into()), ""))
    );
  }

  #[test]
  fn test_parse_bool() {
    assert_eq!(
      parse("true").expect("Failed to parse bool true"),
      Some((LishpValue::Bool(true), ""))
    );
    assert_eq!(
      parse("false").expect("Failed to parse bool false"),
      Some((LishpValue::Bool(false), ""))
    );
  }

  #[test]
  fn test_parse_nil() {
    assert_eq!(
      parse("nil").expect("Failed to parse nil"),
      Some((LishpValue::Nil, ""))
    );
  }

  #[test]
  fn test_parse_symbol() {
    assert_eq!(
      parse("+").expect("Failed to parse symbol +"),
      Some((LishpValue::Symbol { name: "+".into() }, ""))
    );
    assert_eq!(
      parse("define").expect("Failed to parse symbol define"),
      Some((
        LishpValue::Symbol {
          name: "define".into()
        },
        ""
      ))
    );
  }

  #[test]
  fn test_parse_empty_list() {
    assert_eq!(
      parse("()").expect("Failed to parse empty list"),
      Some((LishpValue::Nil, ""))
    );
  }

  #[test]
  fn test_parse_simple_list() {
    let expected = lishp_list![1, 2, 3];
    assert_eq!(
      parse("(1 2 3)").expect("Failed to parse simple list (1 2 3)"),
      Some((expected, ""))
    );
  }

  #[test]
  fn test_parse_nested_list() {
    let expected = lishp_list![sym!("+"), lishp_list![1, 2], 3];
    assert_eq!(
      parse("(+ (1 2) 3)").expect("Failed to parse nested list (+ (1 2) 3)"),
      Some((expected, ""))
    );
  }

  #[test]
  fn test_parse_with_comments() {
    let input = "; This is a comment\n(+ 1 2)";
    let expected = lishp_list![sym!("+"), 1, 2];
    let result = parse(input).expect("Failed to parse expression with comments");
    assert_eq!(result, Some((expected, "")));
  }

  #[test]
  fn test_parse_mixed_types() {
    let input = r#"("hello" 42 true nil)"#;
    let expected = lishp_list!["hello", 42, true, LishpValue::Nil];
    assert_eq!(
      parse(input).expect("Failed to parse mixed types list"),
      Some((expected, ""))
    );
  }

  #[test]
  fn test_parse_empty_input() {
    assert_eq!(parse("").expect("Failed to parse empty string"), None);
    assert_eq!(parse("   ").expect("Failed to parse whitespace only"), None);
    assert_eq!(
      parse("; just a comment\n").expect("Failed to parse comment only"),
      None
    );
  }

  #[test]
  fn test_parse_multiple_expressions() {
    let input = "(+ 1 2) (- 3 4)";
    let (first, remaining) = parse(input)
      .expect("Failed to parse multiple expressions")
      .expect("Expected Some value");
    assert_eq!(first, lishp_list![sym!("+"), 1, 2]);
    assert_eq!(remaining.trim(), "(- 3 4)");
  }

  #[test]
  fn test_incomplete_expression() {
    assert_eq!(parse("(+ 1"), Err(ParseError::Incomplete));
    assert_eq!(parse("(+ 1 2"), Err(ParseError::Incomplete));
    assert_eq!(parse("((+ 1 2)"), Err(ParseError::Incomplete));
  }

  #[test]
  fn test_incomplete_string() {
    assert_eq!(parse("\"hello"), Err(ParseError::Incomplete));
    assert_eq!(parse("(+ \"hello)"), Err(ParseError::Incomplete));
  }

  #[test]
  fn test_balanced_expressions() {
    assert!(parse("(+ 1 2)").is_ok());
    assert!(parse("()").is_ok());
    assert!(parse("\"hello\"").is_ok());
  }

  #[test]
  fn test_unmatched_closing_paren() {
    assert_eq!(parse(")"), Err(ParseError::UnmatchedClosing));
    assert_eq!(parse("(+ 1 2))"), Err(ParseError::UnmatchedClosing));
    assert_eq!(parse(") (+ 1 2)"), Err(ParseError::UnmatchedClosing));
  }

  #[test]
  fn test_parse_special_form() {
    assert_eq!(
      parse("def").expect("Failed to parse special form def"),
      Some((LishpValue::SpecialForm(SpecialForm::Define), ""))
    );
    assert_eq!(
      parse("if").expect("Failed to parse special form if"),
      Some((LishpValue::SpecialForm(SpecialForm::If), ""))
    );
    assert_eq!(
      parse("quote").expect("Failed to parse special form quote"),
      Some((LishpValue::SpecialForm(SpecialForm::Quote), ""))
    );
  }

  #[test]
  fn test_parse_special_form_with_space() {
    assert_eq!(
      parse("def ").expect("Failed to parse 'def '"),
      Some((LishpValue::SpecialForm(SpecialForm::Define), " "))
    );
    assert_eq!(
      parse("if x").expect("Failed to parse 'if x'"),
      Some((LishpValue::SpecialForm(SpecialForm::If), " x"))
    );
  }

  #[test]
  fn test_parse_symbol_not_special_form() {
    assert_eq!(
      parse("define").expect("Failed to parse symbol 'define'"),
      Some((
        LishpValue::Symbol {
          name: "define".into()
        },
        ""
      ))
    );
    assert_eq!(
      parse("defn").expect("Failed to parse symbol 'defn'"),
      Some((
        LishpValue::Symbol {
          name: "defn".into()
        },
        ""
      ))
    );
    assert_eq!(
      parse("iffy").expect("Failed to parse symbol 'iffy'"),
      Some((
        LishpValue::Symbol {
          name: "iffy".into()
        },
        ""
      ))
    );
    assert_eq!(
      parse("letter").expect("Failed to parse symbol 'letter'"),
      Some((
        LishpValue::Symbol {
          name: "letter".into()
        },
        ""
      ))
    );
  }

  #[test]
  fn parse_quote() {
    assert_eq!(
      parse("'(1 2 3)").expect("Failed to parse quote '(1 2 3)"),
      Some((
        LishpValue::Cons(
          Rc::new(LishpValue::SpecialForm(SpecialForm::Quote)),
          Rc::new(LishpValue::Cons(
            Rc::new(lishp_list![1, 2, 3]),
            Rc::new(LishpValue::Nil)
          ))
        ),
        ""
      ))
    );
  }

  #[test]
  fn parse_quote_number() {
    assert_eq!(
      parse("'1").expect("Failed to parse quote '1"),
      Some((
        LishpValue::Cons(
          Rc::new(LishpValue::SpecialForm(SpecialForm::Quote)),
          Rc::new(LishpValue::Cons(
            Rc::new(LishpValue::Integer(1)),
            Rc::new(LishpValue::Nil)
          ))
        ),
        ""
      ))
    );
  }

  #[test]
  fn parse_quote_symbol() {
    assert_eq!(
      parse("'def").expect("Failed to parse quote 'def"),
      Some((
        LishpValue::Cons(
          Rc::new(LishpValue::SpecialForm(SpecialForm::Quote)),
          Rc::new(LishpValue::Cons(
            Rc::new(LishpValue::SpecialForm(SpecialForm::Define)),
            Rc::new(LishpValue::Nil)
          ))
        ),
        ""
      ))
    );

    assert_eq!(
      parse("'asd").expect("Failed to parse quote 'asd"),
      Some((
        LishpValue::Cons(
          Rc::new(LishpValue::SpecialForm(SpecialForm::Quote)),
          Rc::new(LishpValue::Cons(
            Rc::new(LishpValue::Symbol { name: "asd".into() }),
            Rc::new(LishpValue::Nil)
          ))
        ),
        ""
      ))
    );
  }

  #[test]
  fn parse_quote_expression() {
    assert_eq!(
      parse("'(def a 1)").expect("Failed to parse quote '(def a 1)"),
      Some((
        LishpValue::Cons(
          Rc::new(LishpValue::SpecialForm(SpecialForm::Quote)),
          Rc::new(LishpValue::Cons(
            Rc::new(lishp_list![
              LishpValue::SpecialForm(SpecialForm::Define),
              sym!("a"),
              1
            ]),
            Rc::new(LishpValue::Nil)
          ))
        ),
        ""
      ))
    );
  }

  #[test]
  fn parse_empty_quote() {
    assert!(parse("'").is_err());
  }

  #[test]
  fn parse_quote_closing_paren() {
    assert!(parse("(')").is_err());
    assert!(parse("(' )").is_err());
  }

  #[test]
  fn test_invalid_number_with_text() {
    assert!(parse("1t10").is_err());
    assert!(parse("1a").is_err());
    assert!(parse("42abc").is_err());
    assert!(parse("1.5x").is_err());
    assert!(parse("1e10z").is_err());
  }

  #[test]
  fn test_bool_like_symbols() {
    assert_eq!(
      parse("truex").expect("Failed to parse symbol 'truex'"),
      Some((
        LishpValue::Symbol {
          name: "truex".into()
        },
        ""
      ))
    );
    assert_eq!(
      parse("falsea").expect("Failed to parse symbol 'falsea'"),
      Some((
        LishpValue::Symbol {
          name: "falsea".into()
        },
        ""
      ))
    );
  }

  #[test]
  fn test_nil_like_symbols() {
    assert_eq!(
      parse("nilx").expect("Failed to parse symbol 'nilx'"),
      Some((
        LishpValue::Symbol {
          name: "nilx".into()
        },
        ""
      ))
    );
  }

  #[test]
  fn test_valid_number_with_delimiter() {
    assert_eq!(
      parse("42 ").expect("Failed to parse '42 '"),
      Some((LishpValue::Integer(42), " "))
    );
    assert_eq!(
      parse("(42)").expect("Failed to parse '(42)'"),
      Some((lishp_list![42], ""))
    );
    assert_eq!(
      parse("42\n").expect("Failed to parse '42\\n'"),
      Some((LishpValue::Integer(42), "\n"))
    );
    assert_eq!(
      parse("42\t").expect("Failed to parse '42\\t'"),
      Some((LishpValue::Integer(42), "\t"))
    );
  }

  #[test]
  fn test_parse_comma_as_delimiter() {
    // Test comma between numbers
    assert_eq!(
      parse("(1,2,3)").expect("Failed to parse '(1,2,3)'"),
      Some((lishp_list![1, 2, 3], ""))
    );

    // Test comma with spaces
    assert_eq!(
      parse("(1, 2, 3)").expect("Failed to parse '(1, 2, 3)'"),
      Some((lishp_list![1, 2, 3], ""))
    );

    // Test comma in quoted list
    assert_eq!(
      parse("'(1,2,3)").expect("Failed to parse ''(1,2,3)'"),
      Some((
        LishpValue::Cons(
          Rc::new(LishpValue::SpecialForm(SpecialForm::Quote)),
          Rc::new(LishpValue::Cons(
            Rc::new(lishp_list![1, 2, 3]),
            Rc::new(LishpValue::Nil)
          ))
        ),
        ""
      ))
    );

    // Test comma with mixed types
    assert_eq!(
      parse("(\"hello\",42,true)").expect("Failed to parse '(\"hello\",42,true)'"),
      Some((lishp_list!["hello", 42, true], ""))
    );

    // Test multiple commas
    assert_eq!(
      parse("(1,,2)").expect("Failed to parse '(1,,2)'"),
      Some((lishp_list![1, 2], ""))
    );

    // Test trailing comma
    assert_eq!(
      parse("(1,2,)").expect("Failed to parse '(1,2,)'"),
      Some((lishp_list![1, 2], ""))
    );
  }
}
