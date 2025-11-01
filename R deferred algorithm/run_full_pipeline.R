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

# Surface removal-based mitigation output
removal_file <- "removal_suggestions.csv"
if (file.exists(removal_file)) {
  removal_plans <- read_csv(removal_file, show_col_types = FALSE)

  cat("\n--- Removal Scenarios ---\n")
  cat(sprintf("Feasible single removals found: %d\n", nrow(removal_plans)))

  if (nrow(removal_plans) > 0) {
    top_plans <- removal_plans %>%
      arrange(slots_needed_after, desc(conflicts_resolved)) %>%
      head(5)

    apply(top_plans, 1, function(row) {
      cat(sprintf(
        "  Student %s: drop %s → schedule needs %s slots (freed conflicts: %s)\n",
        row[["student_id"]],
        paste0(row[["drop_club_name"]], " (", row[["drop_club_id"]], ")"),
        row[["slots_needed_after"]],
        ifelse(nchar(row[["freed_clubs"]]) > 0, row[["freed_clubs"]], "none")
      ))
      if (!is.na(row[["suggested_alternatives"]]) && nchar(row[["suggested_alternatives"]]) > 0) {
        cat(sprintf("    Suggested options: %s\n", row[["suggested_alternatives"]]))
      }
    })
  }
} else {
  cat("\n(No removal_suggestions.csv file detected. Run the conflict analyzer stage to explore removals.)\n")
}

contact_file <- "students_to_contact.csv"
if (file.exists(contact_file)) {
  outreach <- read_csv(contact_file, show_col_types = FALSE)
  cat("\nStudents needing follow-up: \n")
  cat(sprintf("  %d students have feasible reassignment plans.\n", nrow(outreach)))
  apply(head(outreach, 5), 1, function(row) {
    cat(sprintf(
      "  %s: best drop %s → slots %s (options: %s)\n",
      row[["student_id"]], row[["top_drop"]], row[["best_slots_needed"]],
      ifelse(is.na(row[["top_alternatives"]]) || nchar(row[["top_alternatives"]]) == 0,
             "--", row[["top_alternatives"]])
    ))
  })
} else {
  cat("\n(No students_to_contact.csv file detected. Check the analyzer output.)\n")
}

cat("\nPipeline complete. Review generated CSV files for detailed results.\n")
