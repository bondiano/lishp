use crate::value::LishpValue;
use nom::{
  Err, IResult, Parser,
  branch::alt,
  bytes::complete::{escaped, tag, take_while, take_while1},
  character::complete::{char, multispace0, none_of, one_of},
  combinator::{map, opt, recognize, value},
  multi::many0,
  sequence::{delimited, preceded},
};

fn parse_number(i: &str) -> IResult<&str, LishpValue> {
  map(
    recognize(preceded(
      opt(char('-')),
      take_while1(|c: char| c.is_ascii_digit()),
    )),
    |s: &str| LishpValue::Number(s.parse().expect("Failed to parse validated number string")),
  )
  .parse(i)
}

fn parse_string(i: &str) -> IResult<&str, LishpValue> {
  map(
    delimited(
      char('"'),
      escaped(none_of("\\\""), '\\', one_of("\"nrt\\")),
      char('"'),
    ),
    |s: &str| {
      let unescaped = s
        .replace("\\n", "\n")
        .replace("\\r", "\r")
        .replace("\\t", "\t")
        .replace("\\\\", "\\")
        .replace("\\\"", "\"");
      LishpValue::String(unescaped)
    },
  )
  .parse(i)
}

fn parse_bool(i: &str) -> IResult<&str, LishpValue> {
  alt((
    map(tag("true"), |_| LishpValue::Bool(true)),
    map(tag("false"), |_| LishpValue::Bool(false)),
  ))
  .parse(i)
}

fn parse_nil(i: &str) -> IResult<&str, LishpValue> {
  map(tag("nil"), |_| LishpValue::Nil).parse(i)
}

fn parse_symbol(i: &str) -> IResult<&str, LishpValue> {
  map(
    take_while1(|c: char| {
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
    }),
    |s: &str| LishpValue::Symbol(s.to_string()),
  )
  .parse(i)
}

fn parse_comment(i: &str) -> IResult<&str, ()> {
  value((), preceded(char(';'), take_while(|c| c != '\n'))).parse(i)
}

fn skip_ws_and_comments(i: &str) -> IResult<&str, ()> {
  let (mut input, _) = multispace0(i)?;
  loop {
    match parse_comment(input) {
      Ok((rest, _)) => {
        let (rest, _) = multispace0(rest)?;
        input = rest;
      }
      Err(_) => break,
    }
  }
  Ok((input, ()))
}

fn parse_value(i: &str) -> IResult<&str, LishpValue> {
  preceded(
    skip_ws_and_comments,
    alt((
      parse_nil,
      parse_bool,
      parse_number,
      parse_string,
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
    |values| crate::value::list(values),
  )
  .parse(i)
}

pub fn parse(input: &str) -> Result<Option<(LishpValue, &str)>, String> {
  let trimmed = match skip_ws_and_comments(input) {
    Ok((rest, _)) => rest,
    Err(_) => input,
  };

  if trimmed.is_empty() {
    return Ok(None);
  }

  match parse_value(trimmed) {
    Ok((remaining, value)) => Ok(Some((value, remaining))),
    Err(Err::Error(e)) | Err(Err::Failure(e)) => Err(format!("Parse error at: {:?}", e)),
    Err(Err::Incomplete(_)) => Err("Incomplete input".to_string()),
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::{lishp_list, sym, value::cons};

  #[test]
  fn test_parse_number() {
    assert_eq!(
      parse("42").expect("Failed to parse number 42"),
      Some((LishpValue::Number(42), ""))
    );
    assert_eq!(
      parse("-42").expect("Failed to parse number -42"),
      Some((LishpValue::Number(-42), ""))
    );
    assert_eq!(
      parse("0").expect("Failed to parse number 0"),
      Some((LishpValue::Number(0), ""))
    );
  }

  #[test]
  fn test_parse_string() {
    assert_eq!(
      parse("\"hello\"").expect("Failed to parse string \"hello\""),
      Some((LishpValue::String("hello".to_string()), ""))
    );
    assert_eq!(
      parse("\"hello\\nworld\"").expect("Failed to parse string with escaped newline"),
      Some((LishpValue::String("hello\nworld".to_string()), ""))
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
      Some((LishpValue::Symbol("+".to_string()), ""))
    );
    assert_eq!(
      parse("define").expect("Failed to parse symbol define"),
      Some((LishpValue::Symbol("define".to_string()), ""))
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
}
