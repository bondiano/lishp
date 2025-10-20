use std::fs;
use std::io::{self, BufRead, Write};

/// Trait for abstracting input/output operations
/// This allows using different I/O sources (console, network, browser, etc.)
pub trait IoAdapter {
  /// Read a line of input
  fn read_line(&mut self) -> io::Result<String>;

  /// Print a value to output
  fn print(&mut self, text: &str) -> io::Result<()>;

  /// Print a value followed by a newline
  fn println(&mut self, text: &str) -> io::Result<()> {
    self.print(text)?;
    self.print("\n")
  }

  /// Read contents of a file
  fn read_file(&self, path: &str) -> io::Result<String>;
}

pub struct StdioAdapter {
  stdin: io::Stdin,
  stdout: io::Stdout,
}

impl StdioAdapter {
  pub fn new() -> Self {
    Self {
      stdin: io::stdin(),
      stdout: io::stdout(),
    }
  }
}

impl Default for StdioAdapter {
  fn default() -> Self {
    Self::new()
  }
}

impl IoAdapter for StdioAdapter {
  fn read_line(&mut self) -> io::Result<String> {
    let mut buffer = String::new();
    let mut handle = self.stdin.lock();
    handle.read_line(&mut buffer)?;
    Ok(buffer.trim_end_matches('\n').to_string())
  }

  fn print(&mut self, text: &str) -> io::Result<()> {
    write!(self.stdout, "{}", text)?;
    self.stdout.flush()
  }

  fn read_file(&self, path: &str) -> io::Result<String> {
    fs::read_to_string(path)
  }
}

pub struct MockIoAdapter {
  input: Vec<String>,
  input_position: usize,
  output: Vec<String>,
}

impl MockIoAdapter {
  pub fn new(input: Vec<String>) -> Self {
    Self {
      input,
      input_position: 0,
      output: Vec::new(),
    }
  }

  pub fn output(&self) -> &[String] {
    &self.output
  }
}

impl IoAdapter for MockIoAdapter {
  fn read_line(&mut self) -> io::Result<String> {
    if self.input_position < self.input.len() {
      let line = self.input[self.input_position].clone();
      self.input_position += 1;
      Ok(line)
    } else {
      Err(io::Error::new(
        io::ErrorKind::UnexpectedEof,
        "No more input available",
      ))
    }
  }

  fn print(&mut self, text: &str) -> io::Result<()> {
    self.output.push(text.to_string());
    Ok(())
  }

  fn read_file(&self, _path: &str) -> io::Result<String> {
    Err(io::Error::new(
      io::ErrorKind::Unsupported,
      "File operations not supported in MockIoAdapter",
    ))
  }
}

pub struct StringIoAdapter {
  input: Vec<String>,
  input_position: usize,
  output: String,
}

impl StringIoAdapter {
  pub fn new(input: Vec<String>) -> Self {
    Self {
      input,
      input_position: 0,
      output: String::new(),
    }
  }

  pub fn with_input(input: String) -> Self {
    Self {
      input: vec![input],
      input_position: 0,
      output: String::new(),
    }
  }

  pub fn output_only() -> Self {
    Self {
      input: Vec::new(),
      input_position: 0,
      output: String::new(),
    }
  }

  pub fn output(&self) -> &str {
    &self.output
  }

  pub fn take_output(&mut self) -> String {
    std::mem::take(&mut self.output)
  }
}

impl IoAdapter for StringIoAdapter {
  fn read_line(&mut self) -> io::Result<String> {
    if self.input_position < self.input.len() {
      let line = self.input[self.input_position].clone();
      self.input_position += 1;
      Ok(line)
    } else {
      Err(io::Error::new(
        io::ErrorKind::UnexpectedEof,
        "No more input available",
      ))
    }
  }

  fn print(&mut self, text: &str) -> io::Result<()> {
    self.output.push_str(text);
    Ok(())
  }

  fn read_file(&self, _path: &str) -> io::Result<String> {
    Err(io::Error::new(
      io::ErrorKind::Unsupported,
      "File operations not supported in StringIoAdapter",
    ))
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_string_io_adapter_basic() {
    let mut adapter = StringIoAdapter::new(vec!["test".to_string()]);
    adapter.print("Hello").expect("Failed to print Hello");
    adapter.println("World").expect("Failed to print World");

    assert_eq!(adapter.output(), "HelloWorld\n");
    assert_eq!(adapter.read_line().expect("Failed to read line"), "test");
  }

  #[test]
  fn test_string_io_adapter_take_output() {
    let mut adapter = StringIoAdapter::output_only();
    adapter
      .println("First")
      .expect("Failed to print first line");
    let output = adapter.take_output();
    assert_eq!(output, "First\n");
    assert_eq!(adapter.output(), "");

    adapter
      .println("Second")
      .expect("Failed to print second line");
    assert_eq!(adapter.output(), "Second\n");
  }

  #[test]
  fn test_string_io_adapter_with_input() {
    let mut adapter = StringIoAdapter::with_input("(+ 1 2)".to_string());
    assert_eq!(
      adapter.read_line().expect("Failed to read first line"),
      "(+ 1 2)"
    );

    let result = adapter.read_line();
    assert!(result.is_err());
    assert_eq!(result.unwrap_err().kind(), io::ErrorKind::UnexpectedEof);
  }
}
