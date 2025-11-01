# club_assignment_deferred_acceptance_FIXED.R

library(dplyr)
library(readr)
library(tidyr)
library(stringr)

# --- Settings ---
max_club_capacity <- 15L
required_hours <- 10L
responses_file <- "responses.csv" 
clubs_file <- "clubs.csv"
set.seed(42)

# --- 1. Read and Clean Data ---
responses <- read_csv(responses_file, show_col_types = FALSE)
clubs <- read_csv(clubs_file, show_col_types = FALSE)

# Clean club names
colnames(clubs) <- tolower(trimws(colnames(clubs)))
if("hours_per_week" %in% colnames(clubs)) {
  clubs <- clubs %>% rename(hours = hours_per_week)
}

student_col <- colnames(responses)[1]

# Clean responses
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

# --- 3. Initialize Tracking Structures ---
# Get the student ID type from responses
student_id_type <- class(responses[[student_col]])[1]

# Track current assignments (clubs students are tentatively holding)
current_assignments <- responses %>% 
  select(all_of(student_col)) %>%
  slice(0) %>%
  mutate(
    club_id = character(0),
    student_rank = integer(0)
  )

# Track student hours
student_hours <- responses %>% 
  select(all_of(student_col)) %>%
  mutate(hours_assigned = 0L)

# Track which preferences each student has already tried
attempted_proposals <- responses %>%
  select(all_of(student_col)) %>%
  slice(0) %>%
  mutate(club_id = character(0))

round <- 1L
max_rounds <- nrow(responses_long)

cat(sprintf("Starting Multi-Assignment DA Algorithm\n"))
cat(sprintf("Students: %d, Clubs: %d, Required hours: %d, Max capacity: %d\n\n", 
            nrow(responses), length(active_clubs), required_hours, max_club_capacity))

# --- 4. Main DA Loop ---
while (round <= max_rounds) {
  
  # Find students who need more hours
  students_needing_hours <- student_hours %>%
    filter(hours_assigned < required_hours) %>%
    pull(all_of(student_col))
  
  if (length(students_needing_hours) == 0) {
    cat("All students have reached required hours!\n")
    break
  }
  
  # Get next preferences for students who need hours
  # Exclude clubs they've already tried and clubs they currently hold
  next_proposals <- responses_long %>%
    filter(get(student_col) %in% students_needing_hours) %>%
    anti_join(attempted_proposals, by = c(student_col, "club_id")) %>%
    anti_join(current_assignments, by = c(student_col, "club_id")) %>%
    group_by(across(all_of(student_col))) %>%
    slice_min(student_rank, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  if (nrow(next_proposals) == 0) {
    cat(sprintf("Round %d: No more proposals possible. Stopping.\n", round))
    break
  }
  
  cat(sprintf("--- Round %d: %d proposals ---\n", round, nrow(next_proposals)))
  
  # Mark these proposals as attempted
  attempted_proposals <- attempted_proposals %>%
    bind_rows(next_proposals %>% select(all_of(student_col), club_id))
  
  # --- 5. Club Decision Phase ---
  # Combine new proposals with current assignments
  all_proposals <- current_assignments %>%
    bind_rows(next_proposals %>% select(all_of(student_col), club_id, student_rank))
  
  # Each club ranks all proposals and keeps best 15
  new_assignments <- all_proposals %>%
    group_by(club_id) %>%
    arrange(student_rank, sample(n())) %>%
    slice_head(n = max_club_capacity) %>%
    ungroup()
  
  # Find rejected students (were holding but now rejected)
  rejected <- current_assignments %>%
    anti_join(new_assignments, by = c(student_col, "club_id"))
  
  # Find newly accepted students
  newly_accepted <- new_assignments %>%
    anti_join(current_assignments, by = c(student_col, "club_id"))
  
  cat(sprintf("  Newly accepted: %d, Rejected: %d\n", 
              nrow(newly_accepted), nrow(rejected)))
  
  # Update current assignments
  current_assignments <- new_assignments
  
  # --- 6. Update Student Hours ---
  student_hours <- current_assignments %>%
    left_join(clubs %>% select(club_id, hours), by = "club_id") %>%
    group_by(across(all_of(student_col))) %>%
    summarise(hours_assigned = sum(hours, na.rm = TRUE), .groups = 'drop') %>%
    right_join(responses %>% select(all_of(student_col)), by = student_col) %>%
    replace_na(list(hours_assigned = 0L))
  
  # Students who just completed 10 hours
  completed_this_round <- student_hours %>%
    filter(hours_assigned >= required_hours,
           !get(student_col) %in% 
             (student_hours %>% filter(hours_assigned >= required_hours) %>% pull(all_of(student_col))))
  
  if (nrow(completed_this_round) > 0) {
    cat(sprintf("  Students completing 10 hours: %d\n", nrow(completed_this_round)))
  }
  
  round <- round + 1L
}

# --- 7. Balance Clubs (Ensure Minimum 10 Students) ---
cat("\n=== BALANCING PHASE: Ensuring clubs reach minimum enrollment ===\n")

min_club_size <- 10L
expanded_capacity <- 20L  # Allow clubs to expand to this if needed
balance_round <- 1L
max_balance_rounds <- 5L

while (balance_round <= max_balance_rounds) {
  
  # Check current club enrollments
  club_enrollment <- current_assignments %>%
    group_by(club_id) %>%
    summarise(current_students = n(), .groups = 'drop')
  
  under_enrolled_clubs <- club_enrollment %>%
    filter(current_students < min_club_size) %>%
    pull(club_id)
  
  if (length(under_enrolled_clubs) == 0) {
    cat(sprintf("Balance Round %d: All clubs meet minimum enrollment!\n", balance_round))
    break
  }
  
  cat(sprintf("\nBalance Round %d: %d clubs under minimum (%d students)\n", 
              balance_round, length(under_enrolled_clubs), min_club_size))
  
  # Find students who ranked these under-enrolled clubs but didn't get them
  # Priority: students already at 10 hours who could swap
  potential_swaps <- responses_long %>%
    filter(club_id %in% under_enrolled_clubs) %>%
    # Students who ranked this club but aren't currently in it
    anti_join(current_assignments, by = c(student_col, "club_id")) %>%
    # Add their current hour count
    left_join(student_hours, by = student_col) %>%
    # Prioritize students who are already at 10+ hours (they can afford to swap)
    arrange(student_rank, desc(hours_assigned))
  
  if (nrow(potential_swaps) == 0) {
    cat("  No eligible students for swapping. Cannot balance further.\n")
    break
  }
  
  # Try to add students to under-enrolled clubs
  additions_made <- FALSE
  
  for (club in under_enrolled_clubs) {
    current_size <- club_enrollment %>% 
      filter(club_id == club) %>% 
      pull(current_students)
    
    if (current_size >= min_club_size) next
    
    # How many more students does this club need?
    needed <- min_club_size - current_size
    
    # Get students who want this club
    candidates <- potential_swaps %>%
      filter(club_id == club) %>%
      head(min(needed, expanded_capacity - current_size))
    
    if (nrow(candidates) == 0) next
    
    # Add these students to the club
    for (i in 1:nrow(candidates)) {
      student <- candidates[i,] %>% pull(all_of(student_col))
      rank <- candidates[i,] %>% pull(student_rank)
      
      current_assignments <- current_assignments %>%
        bind_rows(tibble(
          !!student_col := student,
          club_id = club,
          student_rank = rank
        ))
      
      additions_made <- TRUE
    }
    
    cat(sprintf("  Added %d students to %s (was %d, now %d)\n", 
                nrow(candidates), club, current_size, current_size + nrow(candidates)))
  }
  
  if (!additions_made) {
    cat("  No additions possible. Stopping balance phase.\n")
    break
  }
  
  # Recalculate student hours after additions
  student_hours <- current_assignments %>%
    left_join(clubs %>% select(club_id, hours), by = "club_id") %>%
    group_by(across(all_of(student_col))) %>%
    summarise(hours_assigned = sum(hours, na.rm = TRUE), .groups = 'drop') %>%
    right_join(responses %>% select(all_of(student_col)), by = student_col) %>%
    replace_na(list(hours_assigned = 0L))
  
  balance_round <- balance_round + 1L
}

# --- 8. Final Output ---
cat("\n=== FINAL RESULTS ===\n")

# Create final assignments with club names
final_assignments <- current_assignments %>%
  left_join(clubs %>% select(club_id, club_name, hours), by = "club_id") %>%
  select(all_of(student_col), club_id, club_name, hours) %>%
  arrange(across(all_of(student_col)))

# Student summary
student_summary <- final_assignments %>%
  group_by(across(all_of(student_col))) %>%
  summarise(
    clubs_assigned = paste(club_name, collapse = ", "),
    num_clubs = n(),
    hours_assigned = sum(hours, na.rm = TRUE),
    .groups = 'drop'
  )

# Students under 10 hours
students_under_10 <- student_summary %>%
  filter(hours_assigned < required_hours) %>%
  arrange(desc(hours_assigned))

# Clubs under capacity
club_summary <- final_assignments %>%
  group_by(club_id, club_name) %>%
  summarise(students_count = n(), .groups = 'drop') %>%
  arrange(students_count, club_name)

clubs_under_10 <- club_summary %>%
  filter(students_count < 10)

# Print summaries
cat(sprintf("\nTotal students: %d\n", nrow(student_summary)))
cat(sprintf("Students with exactly 10 hours: %d\n", sum(student_summary$hours_assigned == 10)))
cat(sprintf("Students with 10+ hours: %d\n", sum(student_summary$hours_assigned >= 10)))
cat(sprintf("Students with < 10 hours: %d\n", nrow(students_under_10)))
cat(sprintf("\nTotal clubs: %d\n", nrow(club_summary)))
cat(sprintf("Clubs with >= 10 students: %d\n", sum(club_summary$students_count >= 10)))
cat(sprintf("Clubs with < 10 students: %d\n", nrow(clubs_under_10)))

if (nrow(clubs_under_10) > 0) {
  cat("\nClubs still under minimum:\n")
  print(clubs_under_10)
}

if (nrow(students_under_10) > 0) {
  cat("\nStudents under 10 hours (may need manual placement):\n")
  print(students_under_10)
}

# --- 8. Save Files ---
write_csv(final_assignments, "club_assignments_DA_final.csv")
write_csv(student_summary, "student_club_list_DA.csv")
write_csv(students_under_10, "students_under_10_hours_DA.csv")
write_csv(clubs_under_10, "clubs_less_than_10_students_DA.csv")
write_csv(club_summary, "club_summary_DA.csv")

cat("\nFiles saved successfully!\n")
cat("- club_assignments_DA_final.csv (detailed assignments)\n")
cat("- student_club_list_DA.csv (student summary)\n")
cat("- students_under_10_hours_DA.csv (students needing more hours)\n")
cat("- clubs_less_than_10_students_DA.csv (clubs under 10)\n")
cat("- club_summary_DA.csv (all club enrollments)\n")