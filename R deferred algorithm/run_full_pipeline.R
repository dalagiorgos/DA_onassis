#!/usr/bin/env Rscript

# run_full_pipeline.R
# Master orchestration script that runs the improved DA assignment,
# conflict analyzer, and conflict diagnostics in sequence to verify
# scheduling feasibility within 10 slots.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

cat("\n=== CLUB ASSIGNMENT & SCHEDULING PIPELINE ===\n")

# Ensure the working directory is the directory containing this script
args <- commandArgs(trailingOnly = FALSE)
script_path <- NULL
for (arg in args) {
  if (startsWith(arg, "--file=")) {
    script_path <- substring(arg, 8)
    break
  }
}
if (is.null(script_path) || script_path == "") {
  caller <- sys.frames()
  if (length(caller) > 0 && !is.null(caller[[1]]$ofile)) {
    script_path <- caller[[1]]$ofile
  }
}
if (is.null(script_path) || script_path == "") {
  script_path <- file.path(getwd(), "run_full_pipeline.R")
}
script_dir <- dirname(normalizePath(script_path))
setwd(script_dir)
cat(sprintf("Working directory set to: %s\n", script_dir))

# Helper to run each stage and report success/failure
run_stage <- function(label, expr) {
  cat(sprintf("\n--- %s ---\n", label))
  tryCatch({
    eval(expr, envir = parent.frame())
    cat(sprintf("%s completed successfully.\n", label))
    TRUE
  }, error = function(e) {
    cat(sprintf("ERROR during %s: %s\n", label, e$message))
    FALSE
  })
}

# Stage 1: Run the improved Deferred Acceptance algorithm
stage1_ok <- run_stage(
  "Deferred Acceptance Assignment",
  quote(source("claude15.R", local = FALSE))
)

if (!stage1_ok) {
  quit(status = 1)
}

# Stage 2: Analyze conflicts and propose trades
stage2_ok <- run_stage(
  "Conflict Analyzer",
  quote(source("conflictanalyzer.R", local = FALSE))
)

if (!stage2_ok) {
  quit(status = 1)
}

# Stage 3: Run deep diagnostics / feasibility check
stage3_ok <- run_stage(
  "Conflict Diagnostics",
  quote(source("conflictdiagnostics.R", local = FALSE))
)

if (!stage3_ok) {
  quit(status = 1)
}

cat("\n=== SUMMARY ===\n")

# Load outputs if not already available in the global environment
if (!exists("student_summary")) {
  student_summary <- read_csv("student_club_list_DA.csv", show_col_types = FALSE)
}
if (!exists("club_summary")) {
  club_summary <- read_csv("club_summary_DA.csv", show_col_types = FALSE)
}

required_hours <- if (exists("required_hours")) required_hours else 10L
min_club_size <- if (exists("min_club_size")) min_club_size else 10L

students_at_target <- sum(student_summary$hours_assigned == required_hours)
total_students <- nrow(student_summary)
clubs_meeting_min <- sum(club_summary$students_count >= min_club_size)
total_clubs <- nrow(club_summary)

cat(sprintf("Students at %d hours: %d / %d\n", required_hours, students_at_target, total_students))
cat(sprintf("Clubs with ≥%d students: %d / %d\n", min_club_size, clubs_meeting_min, total_clubs))

if (exists("colors_needed")) {
  feasible <- colors_needed <= 10
  cat(sprintf("Graph coloring result: %d colors needed for scheduling.\n", colors_needed))
  if (feasible) {
    cat("✅ All clubs can be scheduled within 10 time slots.\n")
  } else {
    cat(sprintf("❌ Need %d slots but only 10 are available.\n", colors_needed))
  }
} else {
  cat("(Coloring result unavailable. Check diagnostics output.)\n")
}

# Summarize the trade pipeline outputs so administrators can track changes
if (file.exists("trade_proposals.csv")) {
  trade_proposals <- read_csv("trade_proposals.csv", show_col_types = FALSE)
  total_trades <- nrow(trade_proposals)
  total_students_with_trades <- trade_proposals %>% distinct(student_id) %>% nrow()

  cat("\n--- Trade Proposals ---\n")
  cat(sprintf("Total proposed switches: %d (covering %d students)\n",
              total_trades, total_students_with_trades))

  # Highlight a few of the highest-improvement trades so the user can review quickly
  top_trades <- trade_proposals %>%
    arrange(desc(improvement)) %>%
    head(5)

  if (nrow(top_trades) > 0) {
    cat("Top proposed improvements:\n")
    apply(top_trades, 1, function(row) {
      cat(sprintf("  Student %s: swap %s → %s (Δrank +%s)\n",
                  row[["student_id"]], row[["drop_club"]],
                  row[["alternative_club"]], row[["improvement"]]))
    })
  }
} else {
  cat("\n(No trade_proposals.csv file detected. Run the conflict analyzer stage to generate it.)\n")
}

execution_file <- "trade_execution_template.csv"
if (file.exists(execution_file)) {
  execution_log <- read_csv(execution_file, show_col_types = FALSE)
  executed_trades <- execution_log %>%
    filter(!is.na(executed) & executed == TRUE)
  approved_pending <- execution_log %>%
    filter((!is.na(approved) & approved == TRUE) & (is.na(executed) | executed == FALSE))

  cat("\n--- Trade Execution Status ---\n")
  cat(sprintf("Executed trades logged: %d\n", nrow(executed_trades)))
  if (nrow(executed_trades) > 0) {
    apply(head(executed_trades, 5), 1, function(row) {
      cat(sprintf("  Student %s: replaced %s with %s (notes: %s)\n",
                  row[["student_id"]], row[["drop_club"]],
                  row[["alternative_club"]], row[["notes"]]))
    })
  }

  if (nrow(approved_pending) > 0) {
    cat(sprintf("Pending execution (approved but not executed): %d\n",
                nrow(approved_pending)))
  }

  cat(sprintf("Review or update the trade log in: %s\n", execution_file))
} else {
  cat("\n(No trade execution log found. Use trade_execution_template.csv to track approved/complete swaps.)\n")
}

cat("\nPipeline complete. Review generated CSV files for detailed results.\n")
