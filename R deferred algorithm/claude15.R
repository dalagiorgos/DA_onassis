# club_assignment_deferred_acceptance_IMPROVED.R

library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(purrr)

# --- Settings ---
min_club_size <- 10L      # Hard minimum
max_club_capacity <- 22L  # Hard maximum
required_hours <- 10L
responses_file <- "responses.csv" 
clubs_file <- "clubs.csv"
set.seed(42)

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
      # Enforce hard limits: min=10, max=22
      desired_capacity = pmin(pmax(desired_capacity, min_club_size), max_club_capacity)
    )
  cat("Using club-specific desired capacities from clubs.csv\n")
} else {
  # Default to 15 if column doesn't exist
  clubs <- clubs %>%
    mutate(desired_capacity = 15L)
  cat("No club_students column found. Using default capacity of 15 for all clubs.\n")
}

# Validate capacities
if (any(clubs$desired_capacity < min_club_size | clubs$desired_capacity > max_club_capacity)) {
  cat("WARNING: Some club capacities were adjusted to fit [10, 22] range\n")
}

cat(sprintf("Club capacity range: %d to %d students\n", 
            min(clubs$desired_capacity), max(clubs$desired_capacity)))
cat(sprintf("Average desired capacity: %.1f students\n\n", 
            mean(clubs$desired_capacity)))

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

# --- 3. Initialize Tracking Structures ---
current_assignments <- responses %>% 
  select(all_of(student_col)) %>%
  slice(0) %>%
  mutate(
    club_id = character(0),
    student_rank = integer(0)
  )

student_hours <- responses %>% 
  select(all_of(student_col)) %>%
  mutate(hours_assigned = 0L)

attempted_proposals <- responses %>%
  select(all_of(student_col)) %>%
  slice(0) %>%
  mutate(club_id = character(0))

round <- 1L
max_rounds <- nrow(responses_long)

cat(sprintf("Starting Improved DA Algorithm\n"))
cat(sprintf("Students: %d, Clubs: %d, Required hours: %d, Max capacity: %d\n\n", 
            nrow(responses), length(active_clubs), required_hours, max_club_capacity))

# --- 4. Main DA Loop ---
while (round <= max_rounds) {
  
  students_needing_hours <- student_hours %>%
    filter(hours_assigned < required_hours) %>%
    pull(all_of(student_col))
  
  if (length(students_needing_hours) == 0) {
    cat("All students have reached required hours!\n")
    break
  }
  
  if (round %% 10 == 1) {
    cat(sprintf("Round %d: %d students still need hours\n", 
                round, length(students_needing_hours)))
  }
  
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
    ungroup() %>%
    select(all_of(student_col), club_id, student_rank)
  
  if (nrow(next_proposals) == 0) {
    cat(sprintf("Round %d: No valid proposals possible.\n", round))
    break
  }
  
  attempted_proposals <- attempted_proposals %>%
    bind_rows(next_proposals %>% select(all_of(student_col), club_id))
  
  # --- Club Decision Phase ---
  all_proposals <- current_assignments %>%
    bind_rows(next_proposals %>% select(all_of(student_col), club_id, student_rank))
  
  # Apply club-specific capacities
  new_assignments <- all_proposals %>%
    arrange(club_id, student_rank, sample(n())) %>%
    group_split(club_id) %>%
    map_dfr(function(club_data) {
      club <- club_data$club_id[1]
      capacity <- clubs %>% filter(club_id == club) %>% pull(desired_capacity)
      if (length(capacity) == 0) capacity <- 15L  # fallback
      club_data %>% slice_head(n = capacity)
    })
  
  current_assignments <- new_assignments
  
  # --- Update Student Hours ---
  student_hours <- current_assignments %>%
    left_join(clubs %>% select(club_id, hours), by = "club_id") %>%
    group_by(across(all_of(student_col))) %>%
    summarise(hours_assigned = sum(hours, na.rm = TRUE), .groups = 'drop') %>%
    right_join(responses %>% select(all_of(student_col)), by = student_col) %>%
    replace_na(list(hours_assigned = 0L))
  
  round <- round + 1L
}

cat(sprintf("\nDA Phase complete after %d rounds\n", round - 1))
cat(sprintf("Students with 10 hours: %d / %d\n", 
            sum(student_hours$hours_assigned == required_hours), nrow(student_hours)))

# --- 5. PROGRESSIVE ELIMINATION AND REBALANCING ---
cat("\n=== PROGRESSIVE ELIMINATION PHASE ===\n")

# Calculate club enrollments
club_enrollment <- current_assignments %>%
  group_by(club_id) %>%
  summarise(current_students = n(), .groups = 'drop')

# Clubs that didn't get any students - eliminate immediately
empty_clubs <- setdiff(active_clubs, club_enrollment$club_id)
if (length(empty_clubs) > 0) {
  cat(sprintf("Eliminating %d clubs with 0 students\n", length(empty_clubs)))
}

# Progressive elimination: threshold increases from 2 to 9
for (threshold in 2:9) {
  
  # Find clubs below threshold
  clubs_to_eliminate <- club_enrollment %>%
    filter(current_students > 0, current_students < threshold) %>%
    pull(club_id)
  
  if (length(clubs_to_eliminate) == 0) next
  
  cat(sprintf("\nEliminating %d clubs with <%d students\n", 
              length(clubs_to_eliminate), threshold))
  
  # Remove students from eliminated clubs
  affected_students <- current_assignments %>%
    filter(club_id %in% clubs_to_eliminate) %>%
    pull(all_of(student_col)) %>%
    unique()
  
  cat(sprintf("  %d students affected\n", length(affected_students)))
  
  # Remove these assignments
  current_assignments <- current_assignments %>%
    filter(!club_id %in% clubs_to_eliminate)
  
  # Recalculate student hours
  student_hours <- current_assignments %>%
    left_join(clubs %>% select(club_id, hours), by = "club_id") %>%
    group_by(across(all_of(student_col))) %>%
    summarise(hours_assigned = sum(hours, na.rm = TRUE), .groups = 'drop') %>%
    right_join(responses %>% select(all_of(student_col)), by = student_col) %>%
    replace_na(list(hours_assigned = 0L))
  
  # Re-run mini DA for affected students
  students_needing_hours <- student_hours %>%
    filter(hours_assigned < required_hours) %>%
    pull(all_of(student_col))
  
  if (length(students_needing_hours) == 0) {
    cat("  All affected students reassigned\n")
    next
  }
  
  cat(sprintf("  Reassigning %d students\n", length(students_needing_hours)))
  
  # Reset attempted proposals for affected students
  attempted_proposals <- attempted_proposals %>%
    filter(!get(student_col) %in% affected_students)
  
  mini_round <- 1
  while (mini_round <= 50 && length(students_needing_hours) > 0) {
    
    next_proposals <- responses_long %>%
      filter(get(student_col) %in% students_needing_hours) %>%
      filter(!club_id %in% clubs_to_eliminate) %>%
      anti_join(attempted_proposals, by = c(student_col, "club_id")) %>%
      anti_join(current_assignments, by = c(student_col, "club_id")) %>%
      left_join(student_hours %>% select(all_of(student_col), hours_assigned), 
                by = student_col) %>%
      left_join(clubs %>% select(club_id, hours), by = "club_id") %>%
      filter(hours_assigned + hours <= required_hours) %>%
      group_by(across(all_of(student_col))) %>%
      slice_min(student_rank, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(all_of(student_col), club_id, student_rank)
    
    if (nrow(next_proposals) == 0) break
    
    attempted_proposals <- attempted_proposals %>%
      bind_rows(next_proposals %>% select(all_of(student_col), club_id))
    
    # Get club-specific capacities for mini-DA
    all_proposals <- current_assignments %>%
      bind_rows(next_proposals)
    
    current_assignments <- all_proposals %>%
      arrange(club_id, student_rank, sample(n())) %>%
      group_split(club_id) %>%
      map_dfr(function(club_data) {
        club <- club_data$club_id[1]
        capacity <- clubs %>% filter(club_id == club) %>% pull(desired_capacity)
        if (length(capacity) == 0) capacity <- 15L
        club_data %>% slice_head(n = capacity)
      })
    
    student_hours <- current_assignments %>%
      left_join(clubs %>% select(club_id, hours), by = "club_id") %>%
      group_by(across(all_of(student_col))) %>%
      summarise(hours_assigned = sum(hours, na.rm = TRUE), .groups = 'drop') %>%
      right_join(responses %>% select(all_of(student_col)), by = student_col) %>%
      replace_na(list(hours_assigned = 0L))
    
    students_needing_hours <- student_hours %>%
      filter(hours_assigned < required_hours) %>%
      pull(all_of(student_col))
    
    mini_round <- mini_round + 1
  }
  
  # Update club enrollment for next threshold
  club_enrollment <- current_assignments %>%
    group_by(club_id) %>%
    summarise(current_students = n(), .groups = 'drop')
}

# --- 6. PRIORITY FILLING FOR CLUBS WITH <10 STUDENTS ---
cat("\n=== PRIORITY FILLING PHASE ===\n")

# Find clubs with 8-9 students
priority_clubs <- club_enrollment %>%
  filter(current_students >= 8, current_students < min_club_size) %>%
  pull(club_id)

cat(sprintf("Clubs needing priority filling: %d\n", length(priority_clubs)))

if (length(priority_clubs) > 0) {
  
  # Find students who could help (have 10 hours but ranked priority clubs well)
  for (club in priority_clubs) {
    
    current_size <- club_enrollment %>% 
      filter(club_id == club) %>% 
      pull(current_students)
    
    needed <- min_club_size - current_size
    
    # Find students not in this club who ranked it relatively high
    candidates <- responses_long %>%
      filter(club_id == club, student_rank <= 30) %>%
      anti_join(current_assignments %>% filter(club_id == club), 
                by = c(student_col, "club_id")) %>%
      arrange(student_rank) %>%
      head(needed)
    
    if (nrow(candidates) == 0) next
    
    for (i in 1:nrow(candidates)) {
      student <- candidates[i,] %>% pull(all_of(student_col))
      rank <- candidates[i,] %>% pull(student_rank)
      
      current_assignments <- current_assignments %>%
        bind_rows(tibble(
          !!student_col := student,
          club_id = club,
          student_rank = rank
        ))
    }
    
    club_name <- clubs %>% filter(club_id == club) %>% pull(club_name)
    cat(sprintf("  Added %d students to %s\n", nrow(candidates), club_name))
  }
}

# --- 7. Final Calculations ---
student_hours <- current_assignments %>%
  left_join(clubs %>% select(club_id, hours), by = "club_id") %>%
  group_by(across(all_of(student_col))) %>%
  summarise(hours_assigned = sum(hours, na.rm = TRUE), .groups = 'drop') %>%
  right_join(responses %>% select(all_of(student_col)), by = student_col) %>%
  replace_na(list(hours_assigned = 0L))

final_assignments <- current_assignments %>%
  left_join(clubs %>% select(club_id, club_name, hours), by = "club_id") %>%
  select(all_of(student_col), club_id, club_name, hours) %>%
  arrange(across(all_of(student_col)))

student_summary <- final_assignments %>%
  group_by(across(all_of(student_col))) %>%
  summarise(
    clubs_assigned = paste(club_name, collapse = ", "),
    num_clubs = n(),
    hours_assigned = sum(hours, na.rm = TRUE),
    .groups = 'drop'
  )

club_summary <- final_assignments %>%
  group_by(club_id, club_name) %>%
  summarise(students_count = n(), .groups = 'drop') %>%
  arrange(students_count, club_name)

# --- 8. Final Output ---
cat("\n=== FINAL RESULTS ===\n")
cat(sprintf("Students with exactly 10 hours: %d / %d (%.1f%%)\n", 
            sum(student_summary$hours_assigned == 10), nrow(student_summary),
            100 * sum(student_summary$hours_assigned == 10) / nrow(student_summary)))

# Check how many clubs met their desired capacity
clubs_at_desired <- club_summary %>%
  left_join(clubs %>% select(club_id, desired_capacity), by = "club_id") %>%
  filter(students_count >= min_club_size, students_count <= desired_capacity)

cat(sprintf("Clubs at or near desired capacity: %d / %d\n", 
            nrow(clubs_at_desired), nrow(club_summary)))
cat(sprintf("Clubs with >= %d students: %d / %d\n", 
            min_club_size, sum(club_summary$students_count >= min_club_size), 
            nrow(club_summary)))

clubs_under_10 <- club_summary %>% filter(students_count < min_club_size)
if (nrow(clubs_under_10) > 0) {
  cat("\nClubs still under minimum:\n")
  print(clubs_under_10)
}

students_not_10 <- student_summary %>% filter(hours_assigned != required_hours)
if (nrow(students_not_10) > 0) {
  cat("\nStudents not at 10 hours:\n")
  print(students_not_10)
}

# --- Save Files ---
write_csv(final_assignments, "club_assignments_DA_final.csv")
write_csv(student_summary, "student_club_list_DA.csv")
write_csv(clubs_under_10, "clubs_less_than_10_students_DA.csv")
write_csv(club_summary, "club_summary_DA.csv")

cat("\nFiles saved successfully!\n")