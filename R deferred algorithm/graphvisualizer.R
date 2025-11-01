# graph_visualization.R
# Visualize the club conflict graph

library(dplyr)
library(readr)
library(igraph)

# --- Settings ---
assignments_file <- "club_assignments_DA_final.csv"
clubs_file <- "clubs.csv"

cat("=== CLUB CONFLICT GRAPH VISUALIZATION ===\n\n")

# --- 1. Load Data ---
assignments <- read_csv(assignments_file, show_col_types = FALSE)
clubs <- read_csv(clubs_file, show_col_types = FALSE)

colnames(clubs) <- tolower(trimws(colnames(clubs)))
if("hours_per_week" %in% colnames(clubs)) {
  clubs <- clubs %>% rename(hours = hours_per_week)
}

student_col <- colnames(assignments)[1]

# --- 2. Build Conflict Graph ---
cat("Building conflict graph...\n")

club_pairs <- assignments %>%
  select(all_of(student_col), club_id) %>%
  inner_join(
    assignments %>% select(all_of(student_col), club_id),
    by = student_col,
    relationship = "many-to-many"
  ) %>%
  filter(club_id.x < club_id.y) %>%
  group_by(club_id.x, club_id.y) %>%
  summarise(weight = n(), .groups = 'drop')  # Weight = number of shared students

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

cat(sprintf("  Clubs (vertices): %d\n", vcount(conflict_graph)))
cat(sprintf("  Conflicts (edges): %d\n\n", ecount(conflict_graph)))

# --- 3. Add Node Attributes ---
# Get club info
club_info <- clubs %>%
  filter(club_id %in% V(conflict_graph)$name) %>%
  arrange(match(club_id, V(conflict_graph)$name))

V(conflict_graph)$label <- club_info$club_name
V(conflict_graph)$hours <- club_info$hours

# Calculate degree (number of conflicts)
V(conflict_graph)$degree <- degree(conflict_graph)

# Add edge weights
E(conflict_graph)$weight <- club_pairs$weight

# --- 4. Greedy Coloring ---
cat("Applying greedy coloring...\n")
greedy_colors <- greedy_vertex_coloring(conflict_graph)
V(conflict_graph)$color_num <- greedy_colors
num_colors <- length(unique(greedy_colors))

cat(sprintf("  Colors used: %d\n\n", num_colors))

# Create color palette
if (num_colors <= 10) {
  color_palette <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00",
                     "#ffff33", "#a65628", "#f781bf", "#999999", "#66c2a5")
} else {
  # Use rainbow for many colors
  color_palette <- rainbow(num_colors)
}

V(conflict_graph)$color <- color_palette[greedy_colors]

# --- 5. Visualization 1: Full Graph ---
cat("Creating Visualization 1: Full graph with force-directed layout...\n")

png("conflict_graph_full.png", width = 2400, height = 2400, res = 150)

# Use Fruchterman-Reingold layout (force-directed)
layout_fr <- layout_with_fr(conflict_graph)

par(mar = c(1, 1, 3, 1))
plot(conflict_graph,
     layout = layout_fr,
     vertex.size = 8,
     vertex.label.cex = 0.5,
     vertex.label.color = "black",
     vertex.label.dist = 0,
     vertex.frame.color = "white",
     edge.width = 0.5,
     edge.color = rgb(0, 0, 0, 0.2),
     main = sprintf("Club Conflict Graph (%d clubs, %d conflicts, %d colors needed)", 
                    vcount(conflict_graph), ecount(conflict_graph), num_colors))

# Add legend for colors
legend("bottomright", 
       legend = paste("Slot", 1:min(num_colors, 10)),
       fill = color_palette[1:min(num_colors, 10)],
       cex = 0.8,
       title = "Time Slots")

dev.off()
cat("  Saved: conflict_graph_full.png\n\n")

# --- 6. Visualization 2: Sized by Degree ---
cat("Creating Visualization 2: Node size = number of conflicts...\n")

png("conflict_graph_by_degree.png", width = 2400, height = 2400, res = 150)

# Node size proportional to degree
node_sizes <- 3 + (V(conflict_graph)$degree / max(V(conflict_graph)$degree)) * 15

par(mar = c(1, 1, 3, 1))
plot(conflict_graph,
     layout = layout_fr,
     vertex.size = node_sizes,
     vertex.label = ifelse(V(conflict_graph)$degree > 10, 
                           V(conflict_graph)$label, 
                           ""),  # Only label high-conflict clubs
     vertex.label.cex = 0.6,
     vertex.label.color = "black",
     vertex.label.dist = 1,
     vertex.frame.color = "white",
     edge.width = 0.5,
     edge.color = rgb(0, 0, 0, 0.15),
     main = "Club Conflicts (larger = more conflicts)")

dev.off()
cat("  Saved: conflict_graph_by_degree.png\n\n")

# --- 7. Visualization 3: 4-hour vs 2-hour clubs ---
cat("Creating Visualization 3: Highlighting 4-hour clubs...\n")

png("conflict_graph_by_hours.png", width = 2400, height = 2400, res = 150)

# Different shapes for 4-hour vs 2-hour
vertex_shapes <- ifelse(V(conflict_graph)$hours == 4, "square", "circle")
vertex_colors_hours <- ifelse(V(conflict_graph)$hours == 4, "#e41a1c", "#377eb8")

par(mar = c(1, 1, 3, 1))
plot(conflict_graph,
     layout = layout_fr,
     vertex.size = 10,
     vertex.shape = vertex_shapes,
     vertex.color = vertex_colors_hours,
     vertex.label.cex = 0.5,
     vertex.label.color = "black",
     vertex.label.dist = 0,
     vertex.frame.color = "white",
     edge.width = 0.5,
     edge.color = rgb(0, 0, 0, 0.2),
     main = "Club Conflicts by Hours per Week")

legend("bottomright",
       legend = c("4 hours (needs 2 slots)", "2 hours (needs 1 slot)"),
       pch = c(22, 21),  # square and circle
       pt.bg = c("#e41a1c", "#377eb8"),
       pt.cex = 2,
       cex = 0.9)

dev.off()
cat("  Saved: conflict_graph_by_hours.png\n\n")

# --- 8. Visualization 4: Interactive HTML (optional) ---
if (requireNamespace("visNetwork", quietly = TRUE)) {
  cat("Creating Visualization 4: Interactive HTML version...\n")
  
  library(visNetwork)
  
  # Prepare data for visNetwork
  nodes_vis <- data.frame(
    id = V(conflict_graph)$name,
    label = V(conflict_graph)$label,
    title = paste0(V(conflict_graph)$label, 
                   "<br>Conflicts: ", V(conflict_graph)$degree,
                   "<br>Hours: ", V(conflict_graph)$hours,
                   "<br>Slot: ", greedy_colors),
    color = V(conflict_graph)$color,
    size = 10 + V(conflict_graph)$degree,
    group = greedy_colors
  )
  
  edges_vis <- data.frame(
    from = ends(conflict_graph, E(conflict_graph))[,1],
    to = ends(conflict_graph, E(conflict_graph))[,2],
    value = E(conflict_graph)$weight,
    title = paste("Shared students:", E(conflict_graph)$weight)
  )
  
  vis <- visNetwork(nodes_vis, edges_vis) %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visPhysics(stabilization = TRUE) %>%
    visInteraction(navigationButtons = TRUE) %>%
    visLayout(randomSeed = 42)
  
  visSave(vis, "conflict_graph_interactive.html")
  cat("  Saved: conflict_graph_interactive.html\n")
  cat("  (Open in web browser for interactive exploration)\n\n")
} else {
  cat("  Skipping interactive visualization (install visNetwork package)\n\n")
}

# --- 9. Visualization 5: Community Detection ---
cat("Creating Visualization 5: Community structure...\n")

png("conflict_graph_communities.png", width = 2400, height = 2400, res = 150)

# Detect communities (clusters of highly connected clubs)
communities <- cluster_louvain(conflict_graph)
V(conflict_graph)$community <- membership(communities)

# Color by community
community_colors <- rainbow(length(unique(V(conflict_graph)$community)))
V(conflict_graph)$comm_color <- community_colors[V(conflict_graph)$community]

par(mar = c(1, 1, 3, 1))
plot(conflict_graph,
     layout = layout_fr,
     vertex.size = 8,
     vertex.color = V(conflict_graph)$comm_color,
     vertex.label.cex = 0.5,
     vertex.label.color = "black",
     vertex.label.dist = 0,
     vertex.frame.color = "white",
     edge.width = 0.5,
     edge.color = rgb(0, 0, 0, 0.15),
     main = sprintf("Community Structure (%d communities detected)", 
                    length(unique(V(conflict_graph)$community))))

dev.off()
cat("  Saved: conflict_graph_communities.png\n\n")

# --- 10. Visualization 6: Conflict Heatmap ---
cat("Creating Visualization 6: Conflict intensity heatmap...\n")

# Create adjacency matrix
adj_matrix <- as_adjacency_matrix(conflict_graph, attr = "weight", sparse = FALSE)

# Reorder by degree for better visualization
degree_order <- order(degree(conflict_graph), decreasing = TRUE)
adj_matrix_ordered <- adj_matrix[degree_order, degree_order]

# Plot heatmap
png("conflict_heatmap.png", width = 2400, height = 2400, res = 150)

par(mar = c(5, 5, 3, 2))
image(1:nrow(adj_matrix_ordered), 
      1:ncol(adj_matrix_ordered),
      t(adj_matrix_ordered)[,ncol(adj_matrix_ordered):1],
      col = c("white", heat.colors(20)),
      xlab = "", ylab = "",
      main = "Conflict Intensity Heatmap\n(darker = more shared students)",
      axes = FALSE)

# Add gridlines
abline(h = 1:nrow(adj_matrix_ordered) + 0.5, col = "gray90", lwd = 0.5)
abline(v = 1:ncol(adj_matrix_ordered) + 0.5, col = "gray90", lwd = 0.5)

dev.off()
cat("  Saved: conflict_heatmap.png\n\n")

# --- 11. Summary Statistics ---
cat("=== GRAPH STATISTICS ===\n\n")

cat(sprintf("Vertices (clubs): %d\n", vcount(conflict_graph)))
cat(sprintf("Edges (conflicts): %d\n", ecount(conflict_graph)))
cat(sprintf("Density: %.3f (%.1f%% of possible edges exist)\n", 
            edge_density(conflict_graph),
            edge_density(conflict_graph) * 100))
cat(sprintf("Average degree: %.2f\n", mean(degree(conflict_graph))))
cat(sprintf("Max degree: %d\n", max(degree(conflict_graph))))
cat(sprintf("Chromatic number (greedy): %d\n", num_colors))
cat(sprintf("Communities detected: %d\n", length(unique(V(conflict_graph)$community))))
cat(sprintf("Average clustering coefficient: %.3f\n", transitivity(conflict_graph, type = "average")))

# Identify most problematic clubs
cat("\nMost problematic clubs (highest degree):\n")
problematic <- data.frame(
  club = V(conflict_graph)$label,
  conflicts = V(conflict_graph)$degree,
  hours = V(conflict_graph)$hours,
  time_slot = greedy_colors
) %>%
  arrange(desc(conflicts)) %>%
  head(10)

print(problematic)

cat("\n✓ All visualizations created!\n")
cat("\nFiles generated:\n")
cat("  1. conflict_graph_full.png - Full graph colored by time slot\n")
cat("  2. conflict_graph_by_degree.png - Node size = conflict intensity\n")
cat("  3. conflict_graph_by_hours.png - 4-hour vs 2-hour clubs\n")
cat("  4. conflict_graph_interactive.html - Interactive (if visNetwork installed)\n")
cat("  5. conflict_graph_communities.png - Detected club clusters\n")
cat("  6. conflict_heatmap.png - Conflict intensity matrix\n")
cat("\nTip: Install visNetwork for interactive exploration:\n")
cat("  install.packages('visNetwork')\n")