use colored::*;
use rustyline::config::Configurer;
use rustyline::error::ReadlineError;
use rustyline::history::History;
use rustyline::{DefaultEditor, Result as RustyResult};
use std::env;
use std::path::PathBuf;

use super::eval::{EvalError, process_input};
use super::input::{InputHandler, LineResult, handle_eof, handle_error, handle_interrupt};

const VERSION: &str = env!("CARGO_PKG_VERSION");
const DEFAULT_HISTORY_SIZE: usize = 1000;

pub struct ReplSession {
  editor: DefaultEditor,
  history_file: PathBuf,
  input_handler: InputHandler,
}

impl ReplSession {
  pub fn new() -> RustyResult<Self> {
    let mut editor = DefaultEditor::new()?;

    let history_file = get_history_file();
    let history_size = get_history_size();

    editor.set_max_history_size(history_size)?;

    if let Err(_e) = editor.load_history(&history_file) {
      // It's normal for history file not to exist on first run
    }

    Ok(Self {
      editor,
      history_file,
      input_handler: InputHandler::new(),
    })
  }

  pub fn run(&mut self) -> RustyResult<()> {
    print_banner();

    loop {
      let prompt = self.create_prompt();
      let readline = self.editor.readline(&prompt);

      match readline {
        Ok(line) => {
          if !self.input_handler.is_multiline() {
            if line.trim().is_empty() {
              continue;
            }

            match self.handle_command(&line) {
              CommandResult::Exit => {
                handle_eof();
                break;
              }
              CommandResult::Continue => continue,
              CommandResult::Evaluate => {
                // Fall through to evaluation
              }
            }
          }

          match self.input_handler.handle_line(line, &mut self.editor)? {
            LineResult::Complete(result) => {
              for res in result.lines() {
                println!("{} {}", "=>".bright_green().bold(), res.bright_white());
              }
            }
            LineResult::NeedMore => {
              // Continue reading
            }
            LineResult::Error(msg) => {
              eprintln!("{} {}", "Error:".red().bold(), msg);
            }
          }
        }
        Err(ReadlineError::Interrupted) => {
          handle_interrupt(&mut self.input_handler);
        }
        Err(ReadlineError::Eof) => {
          handle_eof();
          break;
        }
        Err(err) => {
          handle_error(err);
          break;
        }
      }
    }

    if let Err(e) = self.editor.save_history(&self.history_file) {
      eprintln!("Warning: Could not save history: {}", e);
    }

    Ok(())
  }

  fn create_prompt(&self) -> String {
    if self.input_handler.is_multiline() {
      "".to_string()
    } else {
      format!(
        "{}{}❯ ",
        "lishp".bright_cyan().bold(),
        format!("[{}]", self.input_handler.line_number()).bright_black()
      )
    }
  }

  fn handle_command(&mut self, line: &str) -> CommandResult {
    if line.starts_with(":l") || line.starts_with(":load") {
      let file = line.split_whitespace().nth(1).unwrap_or("");
      return self.load_file(file).expect("Failed to load file");
    }

    match line.trim() {
      ":quit" | ":exit" => CommandResult::Exit,
      ":help" => {
        print_help();
        CommandResult::Continue
      }
      ":history" => {
        print_history(&self.editor);
        CommandResult::Continue
      }
      _ => CommandResult::Evaluate,
    }
  }

  fn load_file(&mut self, file: &str) -> Result<CommandResult, String> {
    let mut file = std::fs::File::open(file).map_err(|e| format!("Failed to open file: {}", e))?;
    let mut contents = String::new();
    std::io::Read::read_to_string(&mut file, &mut contents)
      .map_err(|e| format!("Failed to read file: {}", e))?;

    match process_input(&contents, false) {
      Ok(result) => {
        println!("{} {}", "=>".bright_green().bold(), result.bright_white());
        Ok(CommandResult::Continue)
      }
      Err(EvalError::Incomplete) => {
        eprintln!("{} Incomplete input in file", "Error:".red().bold());
        Ok(CommandResult::Continue)
      }
      Err(EvalError::Error(msg)) => {
        eprintln!("{} {}", "Error:".red().bold(), msg);
        Ok(CommandResult::Continue)
      }
    }
  }
}

#[derive(Debug)]
enum CommandResult {
  Exit,
  Continue,
  Evaluate,
}

fn create_banner() -> String {
  format!(
    r#"
╔═════════════════════════════════════════════════════╗
║                                                     ║
║     ██╗     ██╗███████╗██╗  ██╗██████╗              ║
║     ██║     ██║██╔════╝██║  ██║██╔══██╗             ║
║     ██║     ██║███████╗███████║██████╔╝             ║
║     ██║     ██║╚════██║██╔══██║██╔═══╝              ║
║     ███████╗██║███████║██║  ██║██║                  ║
║     ╚══════╝╚═╝╚══════╝╚═╝  ╚═╝╚═╝                  ║
║                                                     ║
║                   Version {}                     ║
║                                                     ║
╚═════════════════════════════════════════════════════╝"#,
    VERSION
  )
}

fn print_banner() {
  println!("{}", create_banner().bright_cyan().bold());
  println!(
    "\n  {} {} | {} {}\n",
    "Type".bright_white(),
    ":help".bright_green().bold(),
    ":quit".bright_green().bold(),
    "to exit".bright_white(),
  );
}

fn print_help() {
  let separator = "━".repeat(50).bright_black().to_string();

  println!("\n{}", separator);
  println!("  {}", "Lishp REPL".bright_cyan().bold());
  println!("{}", separator);

  println!("\n  {}", "Commands".bright_yellow().bold());
  println!("    {:12} {}", ":help".bright_green(), "Show this help");
  println!("    {:12} {}", ":quit".bright_green(), "Exit the REPL");
  println!("    {:12} {}", ":history".bright_green(), "Show history");

  println!("\n  {}", "Navigation".bright_yellow().bold());
  println!("    {:12} {}", "↑/↓".bright_magenta(), "Browse history");
  println!("    {:12} {}", "Ctrl+R".bright_magenta(), "Search history");
  println!("    {:12} {}", "Ctrl+C".bright_magenta(), "Interrupt");
  println!("    {:12} {}", "Ctrl+D".bright_magenta(), "Exit");

  println!("\n{}\n", separator);
}

fn print_history(editor: &DefaultEditor) {
  let history = editor.history();
  let separator = "─".repeat(60);

  println!("\n{}", separator.bright_black());
  println!("{}", "  REPL History  ".bright_cyan().bold());
  println!("{}", separator.bright_black());

  if history.is_empty() {
    println!("  {}", "No history entries yet".bright_black().italic());
  } else {
    let total = history.len();
    let start = total.saturating_sub(20);

    if start > 0 {
      println!("  {} {} entries omitted", "...".bright_black(), start);
    }

    for (i, entry) in history.iter().enumerate().skip(start) {
      println!(
        "  {} {}",
        format!("{:>3}:", i + 1).bright_black(),
        entry.bright_white()
      );
    }

    if total > 20 {
      println!(
        "\n  {} Showing last 20 of {} entries",
        "ℹ".bright_blue(),
        total.to_string().bright_cyan()
      );
    }
  }

  println!("{}\n", separator.bright_black());
}

fn get_history_file() -> PathBuf {
  if let Ok(path) = env::var("LISHP_REPL_HISTORY") {
    return PathBuf::from(path);
  }

  if let Some(mut home) = dirs::home_dir() {
    home.push(".lishp_history");
    return home;
  }

  PathBuf::from(".lishp_history")
}

fn get_history_size() -> usize {
  env::var("LISHP_REPL_HISTORY_SIZE")
    .ok()
    .and_then(|s| s.parse().ok())
    .unwrap_or(DEFAULT_HISTORY_SIZE)
}
