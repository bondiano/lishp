use colored::*;
use lishp::parser;
use rustyline::config::Configurer;
use rustyline::error::ReadlineError;
use rustyline::history::History;
use rustyline::{DefaultEditor, Result as RustyResult};
use std::env;
use std::path::PathBuf;

const VERSION: &str = env!("CARGO_PKG_VERSION");

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

const DEFAULT_HISTORY_SIZE: usize = 1000;

pub struct ReplSession {
  editor: DefaultEditor,
  history_file: PathBuf,
}

impl ReplSession {
  pub fn new() -> RustyResult<Self> {
    let mut editor = DefaultEditor::new()?;

    let history_file = get_history_file();
    let history_size = get_history_size();

    editor.set_max_history_size(history_size)?;

    // Load existing history
    if let Err(_e) = editor.load_history(&history_file) {
      // It's normal for history file not to exist on first run
      // eprintln!("Note: Could not load history file: {}", e);
    }

    Ok(Self {
      editor,
      history_file,
    })
  }

  pub fn run(&mut self) -> RustyResult<()> {
    self.print_banner();

    let mut line_number = 1;
    loop {
      let prompt = format!(
        "{}{}❯ ",
        "lishp".bright_cyan().bold(),
        format!("[{}]", line_number).bright_black()
      );

      let readline = self.editor.readline(&prompt);
      match readline {
        Ok(line) => {
          if line.trim().is_empty() {
            continue;
          }

          match self.handle_command(&line) {
            CommandResult::Exit => {
              self.print_goodbye();
              break;
            }
            CommandResult::Continue => continue,
            CommandResult::Evaluate => {
              self.editor.add_history_entry(line.as_str())?;

              match self.evaluate_expression(&line) {
                Ok(result) => {
                  println!("{} {}", "=>".bright_green().bold(), result.bright_white());
                  line_number += 1;
                }
                Err(_) => {
                  // Error already printed by evaluate_expression via diagnostic reporter
                  // Just continue to next input
                }
              }
            }
          }
        }
        Err(ReadlineError::Interrupted) => {
          println!("{}", "^C".yellow());
          continue;
        }
        Err(ReadlineError::Eof) => {
          self.print_goodbye();
          break;
        }
        Err(err) => {
          eprintln!("{} {:?}", "Error:".red().bold(), err);
          break;
        }
      }
    }

    if let Err(e) = self.editor.save_history(&self.history_file) {
      eprintln!("Warning: Could not save history: {}", e);
    }

    Ok(())
  }

  fn print_banner(&self) {
    println!("{}", create_banner().bright_cyan().bold());
    println!(
      "\n  {} {} | {} {}\n",
      "Type".bright_white(),
      ":help".bright_green().bold(),
      ":quit".bright_green().bold(),
      "to exit".bright_white(),
    );
  }

  fn print_goodbye(&self) {
    println!(
      "\n{} {}",
      "👋".bright_yellow(),
      "Thanks for using Lishp! Goodbye!".bright_cyan().italic()
    );
  }

  fn load_file(&self, file: &str) -> Result<CommandResult, String> {
    let mut file = std::fs::File::open(file).map_err(|e| format!("Failed to open file: {}", e))?;
    let mut contents = String::new();
    std::io::Read::read_to_string(&mut file, &mut contents)
      .map_err(|e| format!("Failed to read file: {}", e))?;
    Ok(CommandResult::Continue)
  }

  fn handle_command(&mut self, line: &str) -> CommandResult {
    if line.starts_with(":l") || line.starts_with(":load") {
      let file = line.split_whitespace().nth(1).unwrap_or("");
      return self.load_file(file).expect("Failed to load file");
    }

    match line.trim() {
      ":quit" | ":exit" => CommandResult::Exit,
      ":help" => {
        self.print_help();
        CommandResult::Continue
      }
      ":history" => {
        self.print_history();
        CommandResult::Continue
      }
      _ => CommandResult::Evaluate,
    }
  }

  fn print_history(&self) {
    let history = self.editor.history();
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

  fn print_help(&self) {
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

  fn evaluate_expression(&mut self, input: &str) -> Result<String, String> {
    match parser::parse(input) {
      Ok(Some((value, _))) => {
        // TODO: Evaluate the parsed value
        Ok(format!("{}", value))
      }
      Ok(None) => Err("Empty input".to_string()),
      Err(e) => Err(format!("Parse error: {}", e)),
    }
  }
}

#[derive(Debug)]
enum CommandResult {
  Exit,
  Continue,
  Evaluate,
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
