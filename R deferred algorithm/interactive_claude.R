# interactive_club_assignment.R
# Teachers manually select students from proposals

library(dplyr)
library(readr)
library(tidyr)
library(stringr)

# --- Settings ---
min_club_size <- 10L
max_club_capacity <- 22L
required_hours <- 10L
responses_file <- "responses.csv"
clubs_file <- "clubs.csv"
set.seed(42)

# File for teacher selections (input from teachers)
teacher_selections_file <- "teacher_selections_round_{round}.csv"

# Current round number (start at 1, increment for each iteration)
current_round <- 1L

cat("=== INTERACTIVE CLUB ASSIGNMENT SYSTEM ===\n\n")
cat("This script generates student proposals for teachers to review.\n")
cat("Teachers will manually select students, then we continue to next round.\n\n")

# --- 1. Read and Clean Data ---
responses <- read_csv(responses_file, show_col_types = FALSE)
clubs <- read_csv(clubs_file, show_col_types = FALSE)

colnames(clubs) <- tolower(trimws(colnames(clubs)))
if("hours_per_week" %in% colnames(clubs)) {
  clubs <- clubs %>% rename(hours = hours_per_week)
}

# Handle club_students column
if("club_students" %in% colnames(clubs)) {
  clubs <- clubs %>%
    mutate(
      desired_capacity = as.integer(club_students),
      desired_capacity = pmin(pmax(desired_capacity, min_club_size), max_club_capacity)
    )
} else {
  clubs <- clubs %>% mutate(desired_capacity = 15L)
}

student_col <- colnames(responses)[1]
active_clubs <- trimws(tolower(colnames(responses)[-1]))
colnames(responses) <- c(student_col, active_clubs)

# --- 2. Create Master Preference List ---
responses_long <- responses %>%
  pivot_longer(cols = -all_of(student_col),
               names_to = "club_id", 
               values_to = "student_rank") %>%
  mutate(
    club_id = trimws(tolower(club_id)),
    student_rank = as.integer(student_rank)
  ) %>%
  filter(!is.na(student_rank)) %>%
  arrange(across(all_of(student_col)), student_rank)

# --- 3. Load or Initialize State ---
state_file <- "assignment_state.rds"

if (file.exists(state_file)) {
  cat("Loading previous state...\n")
  state <- readRDS(state_file)
  current_assignments <- state$current_assignments
  student_hours <- state$student_hours
  attempted_proposals <- state$attempted_proposals
  current_round <- state$round
  cat(sprintf("Resuming from Round %d\n\n", current_round))
} else {
  cat("Starting fresh assignment process...\n\n")
  current_assignments <- responses %>% 
    select(all_of(student_col)) %>%
    slice(0) %>%
    mutate(club_id = character(0), student_rank = integer(0))
  
  student_hours <- responses %>% 
    select(all_of(student_col)) %>%
    mutate(hours_assigned = 0L)
  
  attempted_proposals <- responses %>%
    select(all_of(student_col)) %>%
    slice(0) %>%
    mutate(club_id = character(0))
}

# --- 4. Check if we need teacher input from previous round ---
selections_file <- sprintf(str_replace(teacher_selections_file, "\\{round\\}", "%d"), 
                           current_round - 1)

if (current_round > 1 && file.exists(selections_file)) {
  cat(sprintf("Processing teacher selections from Round %d...\n", current_round - 1))
  
  teacher_decisions <- read_csv(selections_file, show_col_types = FALSE)
  
  # Teacher decisions should have columns: club_id, student_id, selected (TRUE/FALSE)
  accepted_students <- teacher_decisions %>%
    filter(selected == TRUE) %>%
    select(!!student_col := student_id, club_id)
  
  # Add student ranks from responses
  accepted_students <- accepted_students %>%
    left_join(responses_long %>% select(all_of(student_col), club_id, student_rank), 
              by = c(student_col, "club_id"))
  
  # Update current assignments
  current_assignments <- current_assignments %>%
    bind_rows(accepted_students)
  
  # Update student hours
  student_hours <- current_assignments %>%
    left_join(clubs %>% select(club_id, hours), by = "club_id") %>%
    group_by(across(all_of(student_col))) %>%
    summarise(hours_assigned = sum(hours, na.rm = TRUE), .groups = 'drop') %>%
    right_join(responses %>% select(all_of(student_col)), by = student_col) %>%
    replace_na(list(hours_assigned = 0L))
  
  cat(sprintf("  Accepted %d students\n", nrow(accepted_students)))
  cat(sprintf("  Students now at 10 hours: %d / %d\n\n", 
              sum(student_hours$hours_assigned == required_hours), 
              nrow(student_hours)))
}

# --- 5. Generate New Proposals for Current Round ---
cat(sprintf("=== ROUND %d: GENERATING PROPOSALS ===\n\n", current_round))

students_needing_hours <- student_hours %>%
  filter(hours_assigned < required_hours) %>%
  pull(all_of(student_col))

if (length(students_needing_hours) == 0) {
  cat("✓ All students have reached 10 hours! Assignment complete.\n")
  
  # Save final results
  final_assignments <- current_assignments %>%
    left_join(clubs %>% select(club_id, club_name, hours), by = "club_id") %>%
    select(all_of(student_col), club_id, club_name, hours) %>%
    arrange(across(all_of(student_col)))
  
  write_csv(final_assignments, "club_assignments_final.csv")
  cat("\nFinal assignments saved to: club_assignments_final.csv\n")
  
  quit(save = "no")
}

cat(sprintf("Students needing more hours: %d\n\n", length(students_needing_hours)))

# Get next preferences - only clubs that won't exceed 10 hours
next_proposals <- responses_long %>%
  filter(get(student_col) %in% students_needing_hours) %>%
  anti_join(attempted_proposals, by = c(student_col, "club_id")) %>%
  anti_join(current_assignments, by = c(student_col, "club_id")) %>%
  left_join(student_hours %>% select(all_of(student_col), hours_assigned), 
            by = student_col) %>%
  left_join(clubs %>% select(club_id, hours), by = "club_id") %>%
  filter(hours_assigned + hours <= required_hours) %>%
  group_by(across(all_of(student_col))) %>%
  slice_min(student_rank, n = 1, with_ties = FALSE) %>%
  ungroup()

if (nrow(next_proposals) == 0) {
  cat("⚠ No valid proposals possible. Some students cannot reach 10 hours.\n")
  
  problem_students <- student_hours %>%
    filter(hours_assigned < required_hours) %>%
    select(all_of(student_col), hours_assigned)
  
  cat("\nStudents unable to complete:\n")
  print(problem_students)
  
  quit(save = "no")
}

# Mark these as attempted
attempted_proposals <- attempted_proposals %>%
  bind_rows(next_proposals %>% select(all_of(student_col), club_id))

cat(sprintf("Generated %d proposals\n\n", nrow(next_proposals)))

# --- 6. Create Proposal Lists for Teachers ---
cat("Creating proposal lists for teachers...\n\n")

# Group proposals by club
proposals_by_club <- next_proposals %>%
  left_join(clubs %>% select(club_id, club_name, desired_capacity), by = "club_id") %>%
  select(
    club_id,
    club_name,
    student_id = all_of(student_col),
    student_rank,
    desired_capacity
  ) %>%
  arrange(club_id, student_rank)

# Add current hours info for teachers to see
proposals_by_club <- proposals_by_club %>%
  left_join(student_hours, by = setNames(student_col, "student_id"))

# Save master proposal list
output_file <- sprintf("proposals_round_%d_ALL_CLUBS.csv", current_round)
write_csv(proposals_by_club, output_file)
cat(sprintf("✓ Saved: %s\n", output_file))

# Create individual files per club
clubs_with_proposals <- unique(proposals_by_club$club_id)

for (club in clubs_with_proposals) {
  club_proposals <- proposals_by_club %>%
    filter(club_id == club)
  
  club_name_clean <- gsub("[^A-Za-z0-9]", "_", club_proposals$club_name[1])
  club_file <- sprintf("proposals_round_%d_%s.csv", current_round, club_name_clean)
  
  write_csv(club_proposals, club_file)
  
  cat(sprintf("  Club: %s (%s)\n", 
              club_proposals$club_name[1], 
              club))
  cat(sprintf("    Proposals: %d students\n", nrow(club_proposals)))
  cat(sprintf("    Capacity: %d students\n", club_proposals$desired_capacity[1]))
  cat(sprintf("    File: %s\n\n", club_file))
}

# --- 7. Create Template for Teacher Responses ---
teacher_response_template <- proposals_by_club %>%
  mutate(selected = NA) %>%
  select(club_id, club_name, student_id, student_rank, hours_assigned, selected)

template_file <- sprintf("teacher_selections_round_%d_TEMPLATE.csv", current_round)
write_csv(teacher_response_template, template_file)

cat(sprintf("\n✓ Saved response template: %s\n\n", template_file))

# --- 8. Save State and Instructions ---
state <- list(
  current_assignments = current_assignments,
  student_hours = student_hours,
  attempted_proposals = attempted_proposals,
  round = current_round + 1
)
saveRDS(state, state_file)

cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("NEXT STEPS FOR TEACHERS:\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

cat("1. Each teacher should open their club's proposal file:\n")
cat("   proposals_round_X_[ClubName].csv\n\n")

cat("2. Review the proposed students. Each row shows:\n")
cat("   - student_id: The student's ID\n")
cat("   - student_rank: How highly they ranked your club (lower = better)\n")
cat("   - hours_assigned: How many hours they already have\n\n")

cat("3. EITHER:\n")
cat("   A) Edit the TEMPLATE file and mark 'selected = TRUE' for accepted students\n")
cat("   B) Create a new CSV with columns: club_id, student_id, selected\n\n")

cat("4. Save your selections as:\n")
cat(sprintf("   teacher_selections_round_%d.csv\n\n", current_round))

cat("5. ALL teachers must submit their selections to this single file.\n")
cat("   (Combine all selections into one CSV file)\n\n")

cat("6. When ready, run this script again to process selections\n")
cat("   and generate the next round of proposals.\n\n")

cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

cat("CURRENT STATUS:\n")
cat(sprintf("  Round: %d\n", current_round))
cat(sprintf("  Clubs receiving proposals: %d\n", length(clubs_with_proposals)))
cat(sprintf("  Total proposals: %d\n", nrow(proposals_by_club)))
cat(sprintf("  Students at 10 hours: %d / %d\n", 
            sum(student_hours$hours_assigned == required_hours), 
            nrow(student_hours)))
cat(sprintf("  Students needing hours: %d\n", length(students_needing_hours)))

cat("\n✓ Script complete. Waiting for teacher selections.\n")