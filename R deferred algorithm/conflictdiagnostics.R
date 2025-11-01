# conflict_diagnostics.R
# Deep analysis of why scheduling is difficult

library(dplyr)
library(readr)
library(igraph)

assignments_file <- "club_assignments_DA_final.csv"
clubs_file <- "clubs.csv"

cat("=== SCHEDULING CONFLICT DIAGNOSTICS ===\n\n")

# --- Load Data ---
assignments <- read_csv(assignments_file, show_col_types = FALSE)
clubs <- read_csv(clubs_file, show_col_types = FALSE)

colnames(clubs) <- tolower(trimws(colnames(clubs)))
if("hours_per_week" %in% colnames(clubs)) {
  clubs <- clubs %>% rename(hours = hours_per_week)
}

student_col <- colnames(assignments)[1]

# --- Build Conflict Graph ---
club_pairs <- assignments %>%
  select(all_of(student_col), club_id) %>%
  inner_join(
    assignments %>% select(all_of(student_col), club_id),
    by = student_col,
    relationship = "many-to-many"
  ) %>%
  filter(club_id.x < club_id.y) %>%
  distinct(club_id.x, club_id.y)

unique_clubs <- unique(assignments$club_id)

if (nrow(club_pairs) > 0) {
  conflict_graph <- graph_from_data_frame(
    club_pairs,
    directed = FALSE,
    vertices = unique_clubs
  )
} else {
  conflict_graph <- make_empty_graph(n = length(unique_clubs), directed = FALSE)
  V(conflict_graph)$name <- unique_clubs
}

# --- Graph Coloring ---
cat("GRAPH COLORING ANALYSIS:\n\n")

greedy_coloring <- greedy_vertex_coloring(conflict_graph)
colors_needed <- length(unique(greedy_coloring))

cat(sprintf("Colors needed (greedy algorithm): %d\n", colors_needed))
cat(sprintf("Available time slots: 10\n\n"))

if (colors_needed <= 10) {
  cat("✓ SCHEDULING IS FEASIBLE!\n")
  cat(sprintf("  Can schedule all clubs in %d time slots\n\n", colors_needed))
} else {
  cat("✗ SCHEDULING IS NOT FEASIBLE\n")
  cat(sprintf("  Need %d slots but only have 10\n\n", colors_needed))
}

# --- Identify Problem Clubs ---
cat("=== PROBLEM ANALYSIS ===\n\n")

# Find clubs with highest degree (most conflicts)
degrees <- degree(conflict_graph)
degree_df <- tibble(
  club_id = V(conflict_graph)$name,
  num_conflicts = degrees,
  color = greedy_coloring
) %>%
  left_join(clubs %>% select(club_id, club_name, hours), by = "club_id") %>%
  arrange(desc(num_conflicts))

cat("Clubs with most conflicts:\n")
print(head(degree_df %>% select(club_name, hours, num_conflicts, color), 15))

cat("\n\nColor distribution (how many clubs per time slot):\n")
color_dist <- degree_df %>%
  group_by(color) %>%
  summarise(num_clubs = n(), .groups = 'drop') %>%
  arrange(color)
print(color_dist)

# --- Identify Critical Students ---
cat("\n\n=== CRITICAL STUDENTS ===\n\n")

# Calculate how many conflicts each student is involved in
student_conflict_count <- assignments %>%
  select(all_of(student_col), club_id) %>%
  left_join(degree_df %>% select(club_id, num_conflicts), by = "club_id") %>%
  group_by(across(all_of(student_col))) %>%
  summarise(
    num_clubs = n(),
    total_conflict_score = sum(num_conflicts),
    avg_conflict_per_club = mean(num_conflicts),
    max_conflict_club = max(num_conflicts),
    .groups = 'drop'
  ) %>%
  arrange(desc(total_conflict_score))

cat("Students contributing most to conflicts:\n")
cat("(High score = enrolled in highly-conflicted clubs)\n\n")
print(head(student_conflict_count, 20))

# --- Calculate Impact of Removing Students ---
cat("\n\n=== WHAT IF WE REMOVE PROBLEMATIC STUDENTS? ===\n\n")

top_problematic_students <- head(student_conflict_count, 13)$student_id

cat(sprintf("Analyzing impact of removing top 13 students...\n\n"))

# Simulate removal
assignments_without_problematic <- assignments %>%
  filter(!get(student_col) %in% top_problematic_students)

# Rebuild graph
club_pairs_reduced <- assignments_without_problematic %>%
  select(all_of(student_col), club_id) %>%
  inner_join(
    assignments_without_problematic %>% select(all_of(student_col), club_id),
    by = student_col,
    relationship = "many-to-many"
  ) %>%
  filter(club_id.x < club_id.y) %>%
  distinct(club_id.x, club_id.y)

if (nrow(club_pairs_reduced) > 0) {
  conflict_graph_reduced <- graph_from_data_frame(
    club_pairs_reduced,
    directed = FALSE,
    vertices = unique_clubs
  )
} else {
  conflict_graph_reduced <- make_empty_graph(n = length(unique_clubs), directed = FALSE)
  V(conflict_graph_reduced)$name <- unique_clubs
}

greedy_coloring_reduced <- greedy_vertex_coloring(conflict_graph_reduced)
colors_needed_reduced <- length(unique(greedy_coloring_reduced))

cat("BEFORE removing students:\n")
cat(sprintf("  Colors needed: %d\n", colors_needed))
cat(sprintf("  Conflict edges: %d\n\n", nrow(club_pairs)))

cat("AFTER removing top 13 students:\n")
cat(sprintf("  Colors needed: %d\n", colors_needed_reduced))
cat(sprintf("  Conflict edges: %d\n", nrow(club_pairs_reduced)))
cat(sprintf("  Improvement: %d fewer colors, %d fewer conflicts\n\n", 
            colors_needed - colors_needed_reduced,
            nrow(club_pairs) - nrow(club_pairs_reduced)))

if (colors_needed_reduced <= 10) {
  cat("✓ FEASIBLE after removing these students!\n")
  cat("  → Trading these 13 students to different clubs WILL solve the problem\n\n")
} else {
  cat("✗ Still NOT feasible after removing these students\n")
  cat(sprintf("  → Would still need %d slots (have 10)\n", colors_needed_reduced))
  cat("  → Need to trade MORE students or add time slots\n\n")
}

# --- Bottleneck Analysis ---
cat("=== BOTTLENECK ANALYSIS ===\n\n")

# Find clubs that appear in many high-degree conflicts
bottleneck_clubs <- club_pairs %>%
  tidyr::pivot_longer(cols = c(club_id.x, club_id.y), 
                      names_to = "side", values_to = "club_id") %>%
  group_by(club_id) %>%
  summarise(conflict_edges = n(), .groups = 'drop') %>%
  left_join(degree_df %>% select(club_id, club_name, hours), by = "club_id") %>%
  arrange(desc(conflict_edges))

cat("Clubs involved in most pairwise conflicts:\n")
cat("(These clubs are scheduling bottlenecks)\n\n")
print(head(bottleneck_clubs, 15))

# --- Clique Analysis ---
cat("\n\n=== CLIQUE ANALYSIS ===\n\n")
cat("(A clique = set of clubs where ALL pairs conflict)\n\n")

max_clique_size <- tryCatch({
  clique_num(conflict_graph)
}, error = function(e) {
  NA
})

if (!is.na(max_clique_size)) {
  cat(sprintf("Maximum clique size: %d\n", max_clique_size))
  
  if (max_clique_size > 10) {
    cat(sprintf("⚠ Problem: %d clubs all conflict with each other!\n", max_clique_size))
    cat("  These clubs MUST all be in different time slots\n")
    cat(sprintf("  But we only have 10 slots → IMPOSSIBLE\n\n"))
    
    # Try to find the clique
    largest_cliques <- tryCatch({
      largest_cliques(conflict_graph)
    }, error = function(e) {
      list()
    })
    
    if (length(largest_cliques) > 0) {
      clique_clubs <- V(conflict_graph)$name[largest_cliques[[1]]]
      clique_names <- clubs %>% 
        filter(club_id %in% clique_clubs) %>%
        pull(club_name)
      
      cat("The problematic clique includes:\n")
      cat(paste(" -", clique_names, collapse = "\n"))
      cat("\n\n")
      
      # Find which students connect this clique
      clique_students <- assignments %>%
        filter(club_id %in% clique_clubs) %>%
        group_by(across(all_of(student_col))) %>%
        filter(n() >= 2) %>%  # Students in 2+ clique clubs
        summarise(
          num_clique_clubs = n(),
          clique_clubs = paste(club_id, collapse = ", "),
          .groups = 'drop'
        ) %>%
        arrange(desc(num_clique_clubs))
      
      cat("Students responsible for this clique:\n")
      print(clique_students)
    }
  } else {
    cat(sprintf("✓ Largest clique has %d clubs (fits in 10 slots)\n", max_clique_size))
  }
} else {
  cat("(Could not compute maximum clique)\n")
}

# --- Recommendations ---
cat("\n\n=== RECOMMENDATIONS ===\n\n")

if (colors_needed <= 10) {
  cat("✓ Your schedule IS feasible! No action needed.\n")
} else if (colors_needed_reduced <= 10) {
  cat("RECOMMENDATION: Trade those 13 students\n")
  cat("  1. Review trade_proposals.csv from conflict resolver\n")
  cat("  2. Contact those 13 students with trade options\n")
  cat("  3. Get their approval for alternative clubs\n")
  cat("  4. Execute trades\n")
  cat("  5. Re-run feasibility checker\n")
} else {
  cat("RECOMMENDATION: Multiple strategies needed\n")
  cat("  1. Trade the 13 students (reduces conflicts)\n")
  cat("  2. Add more time slots:\n")
  cat("     - Evening slots (6-8pm, 8-10pm)\n")
  cat("     - Weekend slots (Saturday morning/afternoon)\n")
  cat(sprintf("     - Need at least %d total slots\n", colors_needed_reduced))
  cat("  3. OR split popular clubs into sections:\n")
  
  # Find clubs that could be split
  splittable <- degree_df %>%
    filter(num_conflicts >= 8) %>%
    head(5)
  
  if (nrow(splittable) > 0) {
    cat("     Consider splitting:\n")
    for (i in 1:nrow(splittable)) {
      cat(sprintf("       - %s (conflicts with %d clubs)\n",
                  splittable$club_name[i], splittable$num_conflicts[i]))
    }
  }
  
  cat("  4. OR re-run assignment algorithm with max conflicts constraint\n")
}

cat("\n✓ Diagnostics complete.\n")