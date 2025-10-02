use colored::*;
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

  fn handle_command(&mut self, line: &str) -> CommandResult {
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
    let title_style = |text: &str| text.bright_cyan().bold();
    let section_style = |text: &str| text.bright_yellow().bold();
    let command_style = |text: &str| text.bright_green();
    let key_style = |text: &str| text.bright_magenta();
    let desc_style = |text: &str| text.bright_white();
    let separator = "━".repeat(64).bright_black().to_string();

    println!("\n{}", separator);
    println!(
      "  {}",
      title_style("Lishp REPL - Interactive Lisp Environment")
    );
    println!("{}", separator);

    println!("\n{}", section_style("  📚 Commands"));
    self.print_help_item(
      &command_style(":help"),
      &desc_style("Show this help message"),
    );
    self.print_help_item(&command_style(":quit"), &desc_style("Exit the REPL"));
    self.print_help_item(&command_style(":exit"), &desc_style("Exit the REPL"));
    self.print_help_item(
      &command_style(":history"),
      &desc_style("Show command history"),
    );

    println!("\n{}", section_style("  ⌨️  Navigation"));
    self.print_help_item(&key_style("↑/↓"), &desc_style("Browse command history"));
    self.print_help_item(&key_style("Ctrl+R"), &desc_style("Search in history"));
    self.print_help_item(&key_style("Ctrl+C"), &desc_style("Interrupt input"));
    self.print_help_item(&key_style("Ctrl+D"), &desc_style("Exit REPL"));

    println!("\n{}", section_style("  🔧 Built-in Functions"));
    self.print_help_category("Arithmetic", "+ - * / mod rem quot");
    self.print_help_category("Comparison", "= not= < > <= >=");
    self.print_help_category("Logic", "and or not");
    self.print_help_category("Predicates", "zero? pos? neg? even? odd?");
    self.print_help_category("Strings", "str join split upper lower trim");

    println!("\n{}", section_style("  ✨ Examples"));

    self.print_example("Arithmetic", "(+ 1 2 3)", "6");
    self.print_example("Variables", "(def x 42)", "42");
    self.print_example("Conditionals", "(if (> 5 3) \"yes\" \"no\")", "\"yes\"");
    self.print_example("Functions", "((fn [x] (* x x)) 5)", "25");
    self.print_example("Let bindings", "(let [x 10 y 20] (+ x y))", "30");

    println!("\n{}", separator);
    println!(
      "  {}",
      "Start typing expressions to evaluate them!"
        .italic()
        .bright_blue()
    );
    println!("{}\n", separator);
  }

  fn print_help_item(
    &self,
    command: &colored::ColoredString,
    description: &colored::ColoredString,
  ) {
    println!("    {:12} {}", command, description);
  }

  fn print_help_category(&self, name: &str, functions: &str) {
    println!(
      "    {} {}",
      format!("{:12}", name).bright_cyan(),
      functions.bright_white()
    );
  }

  fn print_example(&self, category: &str, code: &str, result: &str) {
    println!("\n    {} :", category.bright_cyan().underline());
    println!(
      "      {} {}",
      code.bright_white(),
      format!("→ {}", result).bright_green()
    );
  }

  fn evaluate_expression(&mut self, input: &str) -> Result<String, String> {
    // Simple echo REPL - just return the input
    Ok(format!("{}", input))
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
