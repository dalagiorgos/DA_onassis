# schedule_conflict_resolver.R
# Identifies students causing conflicts and evaluates removal scenarios

library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(igraph)

# --- Settings ---
assignments_file <- "club_assignments_DA_final.csv"
clubs_file <- "clubs.csv"
responses_file <- "responses.csv"

cat("=== SCHEDULE CONFLICT ANALYZER & REMOVAL PLANNER ===\n\n")

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

assignments <- assignments %>%
  rename(student_id = all_of(student_col))

responses <- responses %>%
  rename(student_id = all_of(student_col))

student_col <- "student_id"

# Get student preferences in long format
student_preferences <- responses %>%
  pivot_longer(cols = -all_of(student_col),
               names_to = "club_id",
               values_to = "student_rank") %>%
  mutate(
    club_id = trimws(tolower(club_id)),
    student_rank = suppressWarnings(as.numeric(student_rank))
  ) %>%
  left_join(clubs %>% select(club_id, club_name, hours), by = "club_id")

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
    shared_students = list(unique(student_id)),
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

# --- 4. Removal-based feasibility exploration ---
cat("=== REMOVAL SCENARIO ANALYSIS ===\n\n")

club_lookup <- clubs %>%
  select(club_id, club_name, hours)

student_pairs <- assignments %>%
  select(student_id, club_id) %>%
  inner_join(
    assignments %>% select(student_id, club_id),
    by = "student_id",
    relationship = "many-to-many"
  ) %>%
  filter(club_id.x < club_id.y)

conflict_counts <- student_pairs %>%
  group_by(club_id.x, club_id.y) %>%
  summarise(num_shared_students = n(), .groups = 'drop') %>%
  mutate(key = paste(club_id.x, club_id.y, sep = "__"))

vertices <- unique(assignments$club_id)

build_graph_from_counts <- function(counts, vertices) {
  if (length(vertices) == 0) {
    return(make_empty_graph())
  }

  if (nrow(counts) > 0) {
    graph_from_data_frame(
      counts %>% select(club_id.x, club_id.y),
      directed = FALSE,
      vertices = vertices
    )
  } else {
    g <- make_empty_graph(n = length(vertices), directed = FALSE)
    V(g)$name <- vertices
    g
  }
}

conflict_graph <- build_graph_from_counts(conflict_counts, vertices)

baseline_colors <- if (vcount(conflict_graph) > 0) {
  coloring <- greedy_vertex_coloring(conflict_graph)
  length(unique(coloring))
} else {
  0
}

cat(sprintf("Baseline greedy coloring requires %d time slots.\n", baseline_colors))

student_conflict_pairs <- student_pairs %>%
  group_by(student_id) %>%
  summarise(conflict_pairs = n(), .groups = 'drop') %>%
  arrange(desc(conflict_pairs))

if (nrow(student_conflict_pairs) == 0) {
  cat("No students create scheduling conflicts.\n")
} else {
  cat("Students contributing most to conflicts:\n")
  for (i in 1:min(15, nrow(student_conflict_pairs))) {
    entry <- student_conflict_pairs[i,]
    cat(sprintf("  %s → %d conflict pairs\n", entry$student_id, entry$conflict_pairs))
  }
}

club_degree <- conflict_counts %>%
  select(club_id.x, club_id.y) %>%
  pivot_longer(cols = c(club_id.x, club_id.y),
               names_to = "edge_end",
               values_to = "club_id") %>%
  group_by(club_id) %>%
  summarise(conflict_degree = n(), .groups = 'drop') %>%
  right_join(club_lookup, by = "club_id") %>%
  mutate(conflict_degree = replace_na(conflict_degree, 0)) %>%
  arrange(desc(conflict_degree))

low_conflict_cutoff <- if (nrow(club_degree) > 0) {
  as.numeric(stats::quantile(club_degree$conflict_degree, probs = 0.4, na.rm = TRUE))
} else {
  0
}

simulate_removal <- function(student_id, drop_club, conflict_counts, student_pairs, vertices) {
  relevant_pairs <- student_pairs %>%
    filter(student_id == !!student_id,
           club_id.x == drop_club | club_id.y == drop_club) %>%
    mutate(
      key = paste(pmin(club_id.x, club_id.y), pmax(club_id.x, club_id.y), sep = "__"),
      other_club = if_else(club_id.x == drop_club, club_id.y, club_id.x)
    )

  if (nrow(relevant_pairs) == 0) {
    return(NULL)
  }

  reductions <- relevant_pairs %>%
    group_by(key) %>%
    summarise(reduction = n(), .groups = 'drop')

  adjusted <- conflict_counts %>%
    left_join(reductions, by = "key") %>%
    mutate(
      reduction = replace_na(reduction, 0L),
      remaining = pmax(0L, num_shared_students - reduction)
    )

  resolved_keys <- adjusted %>%
    filter(remaining == 0 & reduction > 0) %>%
    pull(key)

  remaining_counts <- adjusted %>%
    filter(remaining > 0) %>%
    transmute(club_id.x, club_id.y, num_shared_students = remaining, key)

  graph_after <- build_graph_from_counts(remaining_counts, vertices)

  colors_after <- if (vcount(graph_after) > 0) {
    coloring <- greedy_vertex_coloring(graph_after)
    length(unique(coloring))
  } else {
    0
  }

  list(
    colors = colors_after,
    remaining_counts = remaining_counts,
    resolved_keys = resolved_keys,
    pairs_reduced = sum(reductions$reduction)
  )
}

suggest_alternatives <- function(student_id, drop_club, drop_hours,
                                 preferences, assignments, club_degree,
                                 low_conflict_cutoff, limit = 5) {
  current_clubs <- assignments %>%
    filter(student_id == !!student_id) %>%
    pull(club_id)

  alternatives <- preferences %>%
    filter(student_id == !!student_id,
           !is.na(student_rank),
           club_id != drop_club,
           !club_id %in% current_clubs,
           hours == drop_hours) %>%
    left_join(club_degree %>% select(club_id, conflict_degree), by = "club_id") %>%
    mutate(conflict_degree = replace_na(conflict_degree, 0)) %>%
    arrange(conflict_degree, student_rank)

  if (nrow(alternatives) == 0) {
    return(character(0))
  }

  preferred <- alternatives %>%
    filter(conflict_degree <= low_conflict_cutoff)

  if (nrow(preferred) == 0) {
    preferred <- head(alternatives, limit)
  } else {
    preferred <- head(preferred, limit)
  }

  sprintf("%s (rank %s, conflicts %d)",
          preferred$club_name,
          preferred$student_rank,
          preferred$conflict_degree)
}

removal_scenarios <- tibble()
students_to_review <- student_conflict_pairs %>%
  filter(conflict_pairs > 0)

if (nrow(students_to_review) > 0) {
  for (student in students_to_review$student_id) {
    student_assignments <- assignments %>%
      filter(student_id == !!student)

    for (drop_club in student_assignments$club_id) {
      drop_info <- club_lookup %>% filter(club_id == drop_club)

      sim <- simulate_removal(student, drop_club, conflict_counts, student_pairs, vertices)

      if (is.null(sim)) {
        next
      }

      if (sim$colors <= 10) {
        freed_clubs <- student_pairs %>%
          filter(student_id == !!student,
                 club_id.x == drop_club | club_id.y == drop_club) %>%
          mutate(
            key = paste(pmin(club_id.x, club_id.y), pmax(club_id.x, club_id.y), sep = "__"),
            other_club = if_else(club_id.x == drop_club, club_id.y, club_id.x)
          ) %>%
          filter(key %in% sim$resolved_keys) %>%
          distinct(other_club) %>%
          left_join(club_lookup, by = c("other_club" = "club_id")) %>%
          pull(club_name)

        alternatives <- suggest_alternatives(
          student_id = student,
          drop_club = drop_club,
          drop_hours = drop_info$hours[1],
          preferences = student_preferences,
          assignments = assignments,
          club_degree = club_degree,
          low_conflict_cutoff = low_conflict_cutoff
        )

        slot_improvement <- baseline_colors - sim$colors

        removal_scenarios <- removal_scenarios %>%
          bind_rows(tibble(
            student_id = student,
            drop_club_id = drop_club,
            drop_club_name = drop_info$club_name[1],
            drop_hours = drop_info$hours[1],
            conflicts_resolved = length(freed_clubs),
            conflict_pairs_before = student_conflict_pairs$conflict_pairs[student_conflict_pairs$student_id == student],
            slots_needed_after = sim$colors,
            slot_improvement = slot_improvement,
            freed_clubs = ifelse(length(freed_clubs) > 0, paste(freed_clubs, collapse = "; "), ""),
            suggested_alternatives = ifelse(length(alternatives) > 0, paste(alternatives, collapse = "; "), "")
          ))
      }
    }
  }
}

if (nrow(removal_scenarios) > 0) {
  removal_scenarios <- removal_scenarios %>%
    arrange(slots_needed_after, desc(conflicts_resolved), desc(slot_improvement))

  cat(sprintf("Found %d feasible single removals that lead to ≤10 slots.\n",
              nrow(removal_scenarios)))

  cat("\nTop recommendations:\n")
  for (i in 1:min(10, nrow(removal_scenarios))) {
    plan <- removal_scenarios[i,]
    cat(sprintf("  Student %s → drop %s (%s) → colors: %d\n",
                plan$student_id, plan$drop_club_name, plan$drop_club_id,
                plan$slots_needed_after))
    if (nzchar(plan$freed_clubs)) {
      cat(sprintf("    Frees conflicts with: %s\n", plan$freed_clubs))
    }
    if (nzchar(plan$suggested_alternatives)) {
      cat(sprintf("    Suggested new clubs: %s\n", plan$suggested_alternatives))
    }
  }

  write_csv(removal_scenarios, "removal_suggestions.csv")
  cat("\n✓ Removal suggestions saved to: removal_suggestions.csv\n")

  students_to_contact <- removal_scenarios %>%
    group_by(student_id) %>%
    summarise(
      options = n(),
      best_slot_improvement = max(slot_improvement),
      best_slots_needed = min(slots_needed_after),
      top_drop = drop_club_name[which.min(slots_needed_after)],
      top_alternatives = first(suggested_alternatives[suggested_alternatives != ""]),
      .groups = 'drop'
    ) %>%
    arrange(best_slots_needed, desc(best_slot_improvement))

  write_csv(students_to_contact, "students_to_contact.csv")
  cat("✓ Student outreach list saved to: students_to_contact.csv\n")
} else {
  cat("No single student removals reduced the schedule to 10 slots or fewer.\n")
  cat("Consider exploring multi-student adjustments or increasing time slots.\n")
}

cat("\n✓ Analysis complete.\n")
