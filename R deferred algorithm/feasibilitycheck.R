# schedule_feasibility_checker.R
# Checks if a valid schedule exists for club assignments

library(dplyr)
library(readr)
library(igraph)

# --- Settings ---
assignments_file <- "club_assignments_DA_final.csv"
clubs_file <- "clubs.csv"

# Time slots available (Monday-Friday, 2 slots per day)
# Each slot is 2 consecutive hours
time_slots <- c(
  "Mon_Slot1", "Mon_Slot2",
  "Tue_Slot1", "Tue_Slot2",
  "Wed_Slot1", "Wed_Slot2",
  "Thu_Slot1", "Thu_Slot2",
  "Fri_Slot1", "Fri_Slot2"
)

# For display purposes, you can specify actual times
slot_times <- c(
  "Monday 14:00-16:00", "Monday 16:00-18:00",
  "Tuesday 14:00-16:00", "Tuesday 16:00-18:00",
  "Wednesday 14:00-16:00", "Wednesday 16:00-18:00",
  "Thursday 14:00-16:00", "Thursday 16:00-18:00",
  "Friday 14:00-16:00", "Friday 16:00-18:00"
)

cat("=== CLUB SCHEDULE FEASIBILITY CHECKER ===\n\n")

# --- 1. Load Data ---
assignments <- read_csv(assignments_file, show_col_types = FALSE)
clubs <- read_csv(clubs_file, show_col_types = FALSE)

# Clean column names
colnames(clubs) <- tolower(trimws(colnames(clubs)))
if("hours_per_week" %in% colnames(clubs)) {
  clubs <- clubs %>% rename(hours = hours_per_week)
}

student_col <- colnames(assignments)[1]

# Get unique clubs
unique_clubs <- assignments %>%
  distinct(club_id, club_name) %>%
  left_join(clubs %>% select(club_id, hours), by = "club_id")

cat(sprintf("Students: %d\n", length(unique(assignments[[student_col]]))))
cat(sprintf("Clubs to schedule: %d\n", nrow(unique_clubs)))
cat(sprintf("Available time slots: %d\n\n", length(time_slots)))

# --- 2. Build Conflict Graph ---
cat("Building conflict graph...\n")

# Two clubs conflict if they share at least one student
club_pairs <- assignments %>%
  select(all_of(student_col), club_id) %>%
  inner_join(
    assignments %>% select(all_of(student_col), club_id),
    by = student_col,
    relationship = "many-to-many"
  ) %>%
  filter(club_id.x < club_id.y) %>%  # Avoid duplicates
  distinct(club_id.x, club_id.y)

cat(sprintf("  Found %d club pairs that conflict (share students)\n\n", nrow(club_pairs)))

# Create igraph object
if (nrow(club_pairs) > 0) {
  conflict_graph <- graph_from_data_frame(
    club_pairs,
    directed = FALSE,
    vertices = unique_clubs$club_id
  )
} else {
  # No conflicts - all clubs can be scheduled at the same time!
  conflict_graph <- make_empty_graph(n = nrow(unique_clubs), directed = FALSE)
  V(conflict_graph)$name <- unique_clubs$club_id
}

# --- 3. Check for 4-hour clubs ---
cat("Analyzing club hour requirements...\n")

four_hour_clubs <- unique_clubs %>% filter(hours == 4)
two_hour_clubs <- unique_clubs %>% filter(hours == 2)

cat(sprintf("  4-hour clubs: %d (need 2 time slots each)\n", nrow(four_hour_clubs)))
cat(sprintf("  2-hour clubs: %d (need 1 time slot each)\n\n", nrow(two_hour_clubs)))

# For 4-hour clubs, they need TWO time slots
# They could meet 2x per week (2 hours each) OR 1x per week (4 hours - but this violates "2 consecutive hours")
# Assuming 4-hour clubs meet TWICE per week for 2 hours each

# Calculate required slot-assignments
required_slot_assignments <- nrow(two_hour_clubs) + (2 * nrow(four_hour_clubs))

cat(sprintf("Total slot assignments needed: %d\n", required_slot_assignments))
cat(sprintf("Available slots: %d\n\n", length(time_slots)))

if (required_slot_assignments > length(time_slots)) {
  cat("⚠ WARNING: Not enough time slots for all clubs!\n")
  cat(sprintf("  Need at least %d slots but only %d available.\n\n", 
              required_slot_assignments, length(time_slots)))
}

# --- 4. Graph Coloring Check ---
cat("=== FEASIBILITY ANALYSIS ===\n\n")

# Calculate chromatic number (minimum colors needed)
# This is NP-complete, but we can get bounds

# Lower bound: size of maximum clique
max_clique_size <- tryCatch({
  clique_num(conflict_graph)
}, error = function(e) {
  cat("  (Could not compute exact clique number, using approximation)\n")
  return(NA)
})

# Upper bound: greedy coloring
greedy_coloring <- tryCatch({
  greedy_vertex_coloring(conflict_graph)
}, error = function(e) {
  rep(1, vcount(conflict_graph))
})

colors_used <- length(unique(greedy_coloring))

cat("GRAPH COLORING ANALYSIS:\n")
if (!is.na(max_clique_size)) {
  cat(sprintf("  Minimum colors needed (lower bound): %d\n", max_clique_size))
}
cat(sprintf("  Greedy coloring used: %d colors\n", colors_used))
cat(sprintf("  Available time slots: %d\n\n", length(time_slots)))

# --- 5. Check 4-hour club constraints ---
cat("4-HOUR CLUB CONSTRAINTS:\n")

if (nrow(four_hour_clubs) > 0) {
  four_hour_conflicts <- tibble()
  
  for (i in 1:nrow(four_hour_clubs)) {
    club <- four_hour_clubs$club_id[i]
    
    # Find all clubs this 4-hour club conflicts with
    conflicts <- club_pairs %>%
      filter(club_id.x == club | club_id.y == club) %>%
      mutate(conflicting_club = ifelse(club_id.x == club, club_id.y, club_id.x)) %>%
      pull(conflicting_club)
    
    four_hour_conflicts <- four_hour_conflicts %>%
      bind_rows(tibble(
        club_id = club,
        num_conflicts = length(conflicts)
      ))
  }
  
  cat(sprintf("  Each 4-hour club needs 2 non-conflicting slots\n"))
  cat(sprintf("  Max conflicts for a 4-hour club: %d\n\n", 
              max(four_hour_conflicts$num_conflicts, na.rm = TRUE)))
}

# --- 6. Degree Analysis ---
degrees <- degree(conflict_graph)
max_degree <- max(degrees)

cat("CONFLICT INTENSITY:\n")
cat(sprintf("  Maximum conflicts per club: %d\n", max_degree))
cat(sprintf("  Average conflicts per club: %.1f\n\n", mean(degrees)))

# --- 7. Final Verdict ---
cat("=== FEASIBILITY VERDICT ===\n\n")

feasible <- TRUE
issues <- character(0)

# Check 1: Enough slots?
if (required_slot_assignments > length(time_slots)) {
  feasible <- FALSE
  issues <- c(issues, sprintf("Not enough time slots (%d needed, %d available)", 
                              required_slot_assignments, length(time_slots)))
}

# Check 2: Chromatic number
if (!is.na(max_clique_size) && max_clique_size > length(time_slots)) {
  feasible <- FALSE
  issues <- c(issues, sprintf("Maximum clique size (%d) exceeds available slots (%d)", 
                              max_clique_size, length(time_slots)))
}

# Check 3: Greedy coloring
if (colors_used > length(time_slots)) {
  feasible <- FALSE
  issues <- c(issues, sprintf("Greedy coloring needs %d slots but only %d available", 
                              colors_used, length(time_slots)))
}

# Check 4: 4-hour clubs special case
# A 4-hour club that conflicts with 24+ clubs cannot be scheduled (needs 2 slots, but each slot conflicts)
if (nrow(four_hour_clubs) > 0) {
  problematic_4h <- four_hour_conflicts %>%
    filter(num_conflicts >= length(time_slots) - 1)
  
  if (nrow(problematic_4h) > 0) {
    feasible <- FALSE
    issues <- c(issues, sprintf("%d four-hour clubs have too many conflicts", 
                                nrow(problematic_4h)))
  }
}

if (feasible) {
  cat("✓ SCHEDULE IS FEASIBLE!\n\n")
  cat("A valid schedule can be created where:\n")
  cat("  - Each club meets in 2-hour consecutive blocks\n")
  cat("  - No student has conflicting club times\n")
  cat(sprintf("  - Using at most %d time slots\n\n", colors_used))
  
  cat("Recommended next step: Run a scheduling algorithm to assign specific times.\n")
} else {
  cat("✗ SCHEDULE MAY NOT BE FEASIBLE\n\n")
  cat("Issues detected:\n")
  for (issue in issues) {
    cat(sprintf("  - %s\n", issue))
  }
  cat("\n")
  
  cat("RECOMMENDATIONS:\n")
  cat("  1. Increase available time slots (add evening/weekend slots)\n")
  cat("  2. Re-run assignment algorithm with schedule constraints\n")
  cat("  3. Ask some students to drop conflicting clubs\n")
  cat("  4. Split large clubs into multiple sections\n")
}

# --- 8. Try to Generate Schedule (if feasible) ---
if (feasible && colors_used <= length(time_slots)) {
  cat("\n=== GENERATING SAMPLE SCHEDULE ===\n\n")
  
  # Assign 2-hour clubs to slots based on greedy coloring
  two_hour_schedule <- tibble(
    club_id = V(conflict_graph)$name,
    color = greedy_coloring
  ) %>%
    filter(club_id %in% two_hour_clubs$club_id) %>%
    mutate(time_slot = time_slots[color]) %>%
    left_join(unique_clubs %>% select(club_id, club_name), by = "club_id") %>%
    select(club_name, club_id, time_slot)
  
  # For 4-hour clubs, assign TWO different time slots
  if (nrow(four_hour_clubs) > 0) {
    four_hour_schedule <- tibble()
    
    for (i in 1:nrow(four_hour_clubs)) {
      club <- four_hour_clubs$club_id[i]
      club_name <- four_hour_clubs$club_name[i]
      
      # Find conflicts
      conflicts <- club_pairs %>%
        filter(club_id.x == club | club_id.y == club) %>%
        mutate(conflicting_club = ifelse(club_id.x == club, club_id.y, club_id.x)) %>%
        pull(conflicting_club)
      
      # Get colors of conflicting clubs
      conflict_colors <- greedy_coloring[V(conflict_graph)$name %in% conflicts]
      
      # Find two available colors
      available_colors <- setdiff(1:length(time_slots), conflict_colors)
      
      if (length(available_colors) >= 2) {
        selected_colors <- available_colors[1:2]
        
        four_hour_schedule <- four_hour_schedule %>%
          bind_rows(
            tibble(
              club_name = rep(club_name, 2),
              club_id = rep(club, 2),
              time_slot = time_slots[selected_colors],
              session = c("Session 1", "Session 2")
            )
          )
      } else {
        cat(sprintf("  ⚠ Could not schedule 4-hour club: %s\n", club_name))
      }
    }
    
    # Combine schedules
    full_schedule <- bind_rows(
      two_hour_schedule %>% mutate(session = "Single session"),
      four_hour_schedule
    ) %>%
      arrange(time_slot, club_name)
    
  } else {
    full_schedule <- two_hour_schedule %>%
      mutate(session = "Single session") %>%
      arrange(time_slot, club_name)
  }
  
  # Save schedule
  write_csv(full_schedule, "proposed_schedule.csv")
  
  cat("Sample schedule saved to: proposed_schedule.csv\n\n")
  
  # Show summary
  cat("SCHEDULE SUMMARY:\n")
  schedule_summary <- full_schedule %>%
    group_by(time_slot) %>%
    summarise(num_clubs = n(), .groups = 'drop') %>%
    arrange(time_slot)
  
  print(schedule_summary, n = Inf)
  
  cat("\nNote: This is ONE possible schedule. Other arrangements may also work.\n")
}

cat("\n✓ Analysis complete.\n")