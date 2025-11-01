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

# --- 7. Balance Clubs (Ensure Minimum 10 Students) via Swaps ---
cat("\n=== BALANCING PHASE: Ensuring clubs reach minimum enrollment ===\n")

min_club_size <- 10L
balance_round <- 1L
max_balance_rounds <- 10L

# Track club enrollments across rounds to detect no-progress
previous_under_enrolled_count <- Inf

while (balance_round <= max_balance_rounds) {
  
  cat(sprintf("\n--- Balance Round %d ---\n", balance_round))
  
  # Check current club enrollments
  club_enrollment <- current_assignments %>%
    group_by(club_id) %>%
    summarise(current_students = n(), .groups = 'drop')
  
  under_enrolled_clubs <- club_enrollment %>%
    filter(current_students < min_club_size) %>%
    arrange(current_students)
  
  num_under_enrolled <- nrow(under_enrolled_clubs)
  
  cat(sprintf("Clubs under minimum: %d\n", num_under_enrolled))
  
  if (num_under_enrolled == 0) {
    cat("All clubs meet minimum enrollment!\n")
    break
  }
  
  # Check if we're making progress
  if (num_under_enrolled >= previous_under_enrolled_count) {
    cat("No progress made in this round. Stopping.\n")
    break
  }
  previous_under_enrolled_count <- num_under_enrolled
  
  # Show which clubs are under
  cat("Under-enrolled clubs:\n")
  print(under_enrolled_clubs %>% 
          left_join(clubs %>% select(club_id, club_name), by = "club_id") %>%
          select(club_name, current_students))
  
  swaps_made <- 0
  clubs_checked <- 0
  
  # For each under-enrolled club, try to find students who can swap
  for (target_club in under_enrolled_clubs$club_id) {
    clubs_checked <- clubs_checked + 1
    
    current_size <- club_enrollment %>% 
      filter(club_id == target_club) %>% 
      pull(current_students)
    
    if (current_size >= min_club_size) next
    
    target_club_hours <- clubs %>% 
      filter(club_id == target_club) %>% 
      pull(hours)
    
    # Find students who ranked this club but aren't in it
    candidates <- responses_long %>%
      filter(club_id == target_club) %>%
      anti_join(current_assignments, by = c(student_col, "club_id")) %>%
      arrange(student_rank) %>%
      head(50)  # Limit candidates
    
    if (nrow(candidates) == 0) {
      cat(sprintf("  %s: No candidates found\n", target_club))
      next
    }
    
    # For each candidate, check if they can swap a current club
    swaps_this_club <- 0
    for (i in 1:nrow(candidates)) {
      if (current_size >= min_club_size) break
      if (swaps_this_club >= 3) break  # Limit swaps per club per round
      
      student <- candidates[i,] %>% pull(all_of(student_col))
      target_rank <- candidates[i,] %>% pull(student_rank)
      
      # Get student's current clubs
      student_current_clubs <- current_assignments %>%
        filter(get(student_col) == student) %>%
        left_join(clubs %>% select(club_id, hours), by = "club_id") %>%
        left_join(responses_long %>% 
                    filter(get(student_col) == student) %>%
                    select(club_id, current_rank = student_rank), 
                  by = "club_id")
      
      # Find a club they can swap out (same hours, ranked worse)
      swappable_clubs <- student_current_clubs %>%
        filter(hours == target_club_hours, current_rank > target_rank) %>%
        left_join(club_enrollment, by = "club_id") %>%
        arrange(desc(current_students), desc(current_rank))
      
      if (nrow(swappable_clubs) == 0) next
      
      # Perform the swap
      club_to_remove <- swappable_clubs[1,] %>% pull(club_id)
      
      # Remove old assignment
      current_assignments <- current_assignments %>%
        filter(!(get(student_col) == student & club_id == club_to_remove))
      
      # Add new assignment
      current_assignments <- current_assignments %>%
        bind_rows(tibble(
          !!student_col := student,
          club_id = target_club,
          student_rank = target_rank
        ))
      
      # Update tracking
      current_size <- current_size + 1
      club_enrollment <- club_enrollment %>%
        mutate(current_students = if_else(club_id == target_club, 
                                          current_students + 1L, 
                                          current_students),
               current_students = if_else(club_id == club_to_remove, 
                                          current_students - 1L, 
                                          current_students))
      
      swaps_made <- swaps_made + 1
      swaps_this_club <- swaps_this_club + 1
    }
    
    if (swaps_this_club > 0) {
      cat(sprintf("  %s: Added %d students\n", target_club, swaps_this_club))
    }
  }
  
  cat(sprintf("Total swaps: %d, Clubs checked: %d\n", swaps_made, clubs_checked))
  
  if (swaps_made == 0) {
    cat("No swaps possible. Stopping.\n")
    break
  }
  
  balance_round <- balance_round + 1L
}

# Verify all students still have exactly 10 hours
student_hours <- current_assignments %>%
  left_join(clubs %>% select(club_id, hours), by = "club_id") %>%
  group_by(across(all_of(student_col))) %>%
  summarise(hours_assigned = sum(hours, na.rm = TRUE), .groups = 'drop') %>%
  right_join(responses %>% select(all_of(student_col)), by = student_col) %>%
  replace_na(list(hours_assigned = 0L))

# Check for any violations
hours_violations <- student_hours %>%
  filter(hours_assigned != required_hours)

if (nrow(hours_violations) > 0) {
  cat("\n!!! WARNING: Some students don't have exactly 10 hours after balancing !!!\n")
  print(hours_violations)
} else {
  cat("\n✓ All students have exactly 10 hours\n")
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