# schedule_conflict_resolver.R
# Identifies students causing conflicts and proposes trades

library(dplyr)
library(readr)
library(tidyr)
library(stringr)

# --- Settings ---
assignments_file <- "club_assignments_DA_final.csv"
clubs_file <- "clubs.csv"
responses_file <- "responses.csv"

cat("=== SCHEDULE CONFLICT ANALYZER & TRADE PROPOSER ===\n\n")

# --- 1. Load Data ---
assignments <- read_csv(assignments_file, show_col_types = FALSE)
clubs <- read_csv(clubs_file, show_col_types = FALSE)
responses <- read_csv(responses_file, show_col_types = FALSE)

# Clean column names
colnames(clubs) <- tolower(trimws(colnames(clubs)))
if("hours_per_week" %in% colnames(clubs)) {
  clubs <- clubs %>% rename(hours = hours_per_week)
}

student_col <- colnames(assignments)[1]
colnames(responses) <- c(student_col, tolower(trimws(colnames(responses)[-1])))

# Get student preferences in long format
student_preferences <- responses %>%
  pivot_longer(cols = -all_of(student_col),
               names_to = "club_id",
               values_to = "student_rank") %>%
  mutate(club_id = trimws(tolower(club_id)))

# --- 2. Identify All Conflicts ---
cat("Analyzing scheduling conflicts...\n\n")

# Find all club pairs that share students
club_conflicts <- assignments %>%
  select(all_of(student_col), club_id) %>%
  inner_join(
    assignments %>% select(all_of(student_col), club_id),
    by = student_col,
    relationship = "many-to-many"
  ) %>%
  filter(club_id.x < club_id.y) %>%
  # Add club names
  left_join(clubs %>% select(club_id, club_name_x = club_name, hours_x = hours),
            by = c("club_id.x" = "club_id")) %>%
  left_join(clubs %>% select(club_id, club_name_y = club_name, hours_y = hours),
            by = c("club_id.y" = "club_id")) %>%
  # Count students per conflict
  group_by(club_id.x, club_id.y, club_name_x, club_name_y, hours_x, hours_y) %>%
  summarise(
    num_shared_students = n(),
    shared_students = list(unique(get(student_col))),
    .groups = 'drop'
  ) %>%
  arrange(desc(num_shared_students))

cat(sprintf("Total conflict pairs: %d\n", nrow(club_conflicts)))
cat(sprintf("Total unique clubs involved: %d\n\n", 
            length(unique(c(club_conflicts$club_id.x, club_conflicts$club_id.y)))))

# --- 3. Prioritize Conflicts ---
cat("=== HIGH-PRIORITY CONFLICTS ===\n\n")
cat("Conflicts involving 4-hour clubs or many students:\n\n")

high_priority <- club_conflicts %>%
  filter(hours_x == 4 | hours_y == 4 | num_shared_students >= 5) %>%
  arrange(desc(num_shared_students))

if (nrow(high_priority) > 0) {
  for (i in 1:min(10, nrow(high_priority))) {
    conflict <- high_priority[i,]
    
    cat(sprintf("CONFLICT #%d:\n", i))
    cat(sprintf("  Clubs: %s (%dh) ↔ %s (%dh)\n", 
                conflict$club_name_x, conflict$hours_x,
                conflict$club_name_y, conflict$hours_y))
    cat(sprintf("  Shared students: %d\n", conflict$num_shared_students))
    cat(sprintf("  Student IDs: %s\n\n", 
                paste(unlist(conflict$shared_students), collapse = ", ")))
  }
} else {
  cat("✓ No high-priority conflicts found!\n\n")
}

# --- 4. For Each Conflict, Propose Trades ---
cat("=== TRADE PROPOSALS ===\n\n")

trade_proposals <- tibble()

for (i in 1:min(20, nrow(club_conflicts))) {
  conflict <- club_conflicts[i,]
  
  club_a <- conflict$club_id.x
  club_b <- conflict$club_id.y
  club_a_name <- conflict$club_name_x
  club_b_name <- conflict$club_name_y
  shared_students <- unlist(conflict$shared_students)
  
  # For each shared student, analyze their preferences
  for (student in shared_students) {
    
    # Get student's current clubs and hours
    student_clubs <- assignments %>%
      filter(get(student_col) == student) %>%
      left_join(clubs %>% select(club_id, club_name, hours), by = "club_id")
    
    total_hours <- sum(student_clubs$hours)
    
    # Get student's rankings for these two conflicting clubs
    rankings <- student_preferences %>%
      filter(get(student_col) == student,
             club_id %in% c(club_a, club_b)) %>%
      arrange(student_rank)
    
    preferred_club <- rankings$club_id[1]
    preferred_rank <- rankings$student_rank[1]
    less_preferred_club <- rankings$club_id[2]
    less_preferred_rank <- rankings$student_rank[2]
    
    preferred_club_name <- if(preferred_club == club_a) club_a_name else club_b_name
    less_preferred_club_name <- if(less_preferred_club == club_a) club_a_name else club_b_name
    
    # Get club hours
    less_preferred_hours <- clubs %>% 
      filter(club_id == less_preferred_club) %>% 
      pull(hours)
    
    # Find alternative clubs student could take instead of less-preferred
    alternatives <- student_preferences %>%
      filter(
        get(student_col) == student,
        student_rank < less_preferred_rank,  # Better than current less-preferred
        student_rank > preferred_rank,       # Worse than most preferred
        !club_id %in% student_clubs$club_id  # Not already in this club
      ) %>%
      left_join(clubs %>% select(club_id, club_name, hours), by = "club_id") %>%
      # Must have same hours to maintain 10 total
      filter(hours == less_preferred_hours) %>%
      arrange(student_rank) %>%
      head(5)
    
    if (nrow(alternatives) > 0) {
      # Propose trade
      for (j in 1:nrow(alternatives)) {
        alt <- alternatives[j,]
        
        trade_proposals <- trade_proposals %>%
          bind_rows(tibble(
            student_id = student,
            conflict_club_a = club_a_name,
            conflict_club_b = club_b_name,
            keep_club = preferred_club_name,
            keep_club_rank = preferred_rank,
            drop_club = less_preferred_club_name,
            drop_club_rank = less_preferred_rank,
            alternative_club = alt$club_name,
            alternative_club_id = alt$club_id,
            alternative_rank = alt$student_rank,
            hours_maintained = less_preferred_hours,
            improvement = less_preferred_rank - alt$student_rank
          ))
      }
    }
  }
}

# --- 5. Display Best Trade Proposals ---
if (nrow(trade_proposals) > 0) {
  
  cat("Found trade proposals to resolve conflicts:\n\n")
  
  # Sort by improvement (most improvement first)
  best_trades <- trade_proposals %>%
    arrange(desc(improvement)) %>%
    head(20)
  
  for (i in 1:nrow(best_trades)) {
    trade <- best_trades[i,]
    
    cat(sprintf("TRADE PROPOSAL #%d:\n", i))
    cat(sprintf("  Student: %s\n", trade$student_id))
    cat(sprintf("  Problem: '%s' conflicts with '%s'\n",
                trade$conflict_club_a, trade$conflict_club_b))
    cat(sprintf("  Student prefers: '%s' (ranked #%d)\n",
                trade$keep_club, trade$keep_club_rank))
    cat(sprintf("  Student less prefers: '%s' (ranked #%d)\n",
                trade$drop_club, trade$drop_club_rank))
    cat(sprintf("  → PROPOSE: Switch '%s' to '%s' (ranked #%d)\n",
                trade$drop_club, trade$alternative_club, trade$alternative_rank))
    cat(sprintf("  Benefit: Student moves UP %d positions in preference\n",
                trade$improvement))
    cat(sprintf("  Hours: %d → %d (maintained)\n\n",
                trade$hours_maintained, trade$hours_maintained))
  }
  
  # --- 6. Save Trade Proposals ---
  write_csv(trade_proposals, "trade_proposals.csv")
  cat("✓ All trade proposals saved to: trade_proposals.csv\n\n")
  
  # --- 7. Conflict Resolution Summary ---
  cat("=== CONFLICT RESOLUTION SUMMARY ===\n\n")
  
  # How many conflicts can be resolved?
  resolvable_conflicts <- trade_proposals %>%
    select(conflict_club_a, conflict_club_b, student_id) %>%
    distinct() %>%
    group_by(conflict_club_a, conflict_club_b) %>%
    summarise(students_with_trades = n(), .groups = 'drop')
  
  conflicts_with_solutions <- club_conflicts %>%
    left_join(
      resolvable_conflicts,
      by = c("club_name_x" = "conflict_club_a", "club_name_y" = "conflict_club_b")
    ) %>%
    replace_na(list(students_with_trades = 0)) %>%
    mutate(
      pct_resolvable = students_with_trades / num_shared_students * 100
    ) %>%
    filter(students_with_trades > 0) %>%
    arrange(desc(pct_resolvable))
  
  cat(sprintf("Conflicts with possible trades: %d / %d\n",
              nrow(conflicts_with_solutions), nrow(club_conflicts)))
  
  if (nrow(conflicts_with_solutions) > 0) {
    cat("\nTop resolvable conflicts:\n")
    for (i in 1:min(10, nrow(conflicts_with_solutions))) {
      conf <- conflicts_with_solutions[i,]
      cat(sprintf("  %s ↔ %s: %d/%d students (%.0f%%)\n",
                  conf$club_name_x, conf$club_name_y,
                  conf$students_with_trades, conf$num_shared_students,
                  conf$pct_resolvable))
    }
  }
  
  # --- 8. Generate Student Contact List ---
  cat("\n\n=== STUDENTS TO CONTACT ===\n\n")
  
  students_to_contact <- trade_proposals %>%
    group_by(student_id) %>%
    summarise(
      num_conflicts = n_distinct(paste(conflict_club_a, conflict_club_b)),
      num_alternatives = n(),
      best_alternative = first(alternative_club),
      best_alternative_rank = first(alternative_rank),
      .groups = 'drop'
    ) %>%
    arrange(desc(num_conflicts))
  
  cat(sprintf("Total students with trade options: %d\n\n", nrow(students_to_contact)))
  
  cat("Priority students (most conflicts):\n")
  for (i in 1:min(15, nrow(students_to_contact))) {
    student <- students_to_contact[i,]
    cat(sprintf("  Student %s: %d conflicts, %d alternatives available\n",
                student$student_id, student$num_conflicts, student$num_alternatives))
  }
  
  write_csv(students_to_contact, "students_to_contact.csv")
  cat("\n✓ Student contact list saved to: students_to_contact.csv\n")
  
  # --- 9. Create Trade Execution Script ---
  cat("\n\n=== AUTOMATED TRADE EXECUTION ===\n\n")
  
  # Create a template for administrators to approve trades
  execution_template <- trade_proposals %>%
    mutate(
      approved = NA,
      executed = NA,
      notes = ""
    ) %>%
    select(
      student_id, 
      drop_club, 
      alternative_club, 
      improvement,
      approved,
      executed,
      notes
    )
  
  write_csv(execution_template, "trade_execution_template.csv")
  
  cat("Trade execution template created: trade_execution_template.csv\n")
  cat("\nTo execute trades:\n")
  cat("  1. Review trade_proposals.csv\n")
  cat("  2. Mark 'approved = TRUE' in trade_execution_template.csv\n")
  cat("  3. Contact students to confirm their agreement\n")
  cat("  4. Run trade executor script (to be created)\n")
  
} else {
  cat("No viable trades found. Conflicts may be irresolvable through trades alone.\n")
  cat("\nConsider:\n")
  cat("  - Adding more time slots (evenings, weekends)\n")
  cat("  - Splitting popular clubs into multiple sections\n")
  cat("  - Re-running assignment with scheduling constraints\n")
}

cat("\n✓ Analysis complete.\n")