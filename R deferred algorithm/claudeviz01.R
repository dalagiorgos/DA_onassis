# visualization_results.R

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

# Read the results
assignments <- read_csv("club_assignments_DA_final.csv", show_col_types = FALSE)
student_summary <- read_csv("student_club_list_DA.csv", show_col_types = FALSE)
club_summary <- read_csv("club_summary_DA.csv", show_col_types = FALSE)
clubs <- read_csv("clubs.csv", show_col_types = FALSE)
responses <- read_csv("responses.csv", show_col_types = FALSE)

# Clean club data
colnames(clubs) <- tolower(trimws(colnames(clubs)))
if("hours_per_week" %in% colnames(clubs)) {
  clubs <- clubs %>% rename(hours = hours_per_week)
}

# Get student column name
student_col <- colnames(responses)[1]

cat("=== OVERALL STATISTICS ===\n\n")

# Student hours distribution
cat("Student Hours Distribution:\n")
hours_dist <- student_summary %>%
  group_by(hours_assigned) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(hours_assigned)
print(hours_dist)

cat(sprintf("\nStudents with exactly 10 hours: %d / %d (%.1f%%)\n", 
            sum(student_summary$hours_assigned == 10),
            nrow(student_summary),
            100 * sum(student_summary$hours_assigned == 10) / nrow(student_summary)))

# Club enrollment distribution
cat("\n\nClub Enrollment Distribution:\n")
enrollment_dist <- club_summary %>%
  mutate(category = case_when(
    students_count < 10 ~ "Under 10",
    students_count >= 10 & students_count <= 15 ~ "10-15 (Target)",
    students_count > 15 ~ "Over 15"
  )) %>%
  group_by(category) %>%
  summarise(count = n(), .groups = 'drop')
print(enrollment_dist)

cat(sprintf("\nClubs with < 10 students: %d / %d\n", 
            sum(club_summary$students_count < 10),
            nrow(club_summary)))

# Show under-enrolled clubs
cat("\n\nUnder-enrolled Clubs (< 10 students):\n")
under_enrolled <- club_summary %>%
  filter(students_count < 10) %>%
  arrange(students_count)
print(under_enrolled)

# Number of clubs per student
cat("\n\nNumber of Clubs per Student:\n")
clubs_per_student <- student_summary %>%
  group_by(num_clubs) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(num_clubs)
print(clubs_per_student)

# Club hours summary
cat("\n\nClub Hours Distribution:\n")
club_hours_summary <- clubs %>%
  group_by(hours) %>%
  summarise(count = n(), .groups = 'drop')
print(club_hours_summary)

# Check 4-hour clubs specifically
cat("\n\n4-Hour Clubs Enrollment:\n")
four_hour_clubs <- club_summary %>%
  left_join(clubs %>% select(club_id, hours), by = "club_id") %>%
  filter(hours == 4) %>%
  select(club_name, students_count, hours)
print(four_hour_clubs)

# === VISUALIZATIONS ===

# 1. Student Hours Distribution
p1 <- ggplot(student_summary, aes(x = hours_assigned)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = 10, color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Student Hours Distribution",
       subtitle = "Red line = target 10 hours",
       x = "Hours Assigned",
       y = "Number of Students") +
  theme_minimal()

ggsave("plot_student_hours.png", p1, width = 10, height = 6)
cat("\nSaved: plot_student_hours.png\n")

# 2. Club Enrollment Distribution
p2 <- ggplot(club_summary, aes(x = reorder(club_name, students_count), y = students_count)) +
  geom_col(aes(fill = students_count < 10), show.legend = FALSE) +
  geom_hline(yintercept = 10, color = "red", linetype = "dashed") +
  geom_hline(yintercept = 15, color = "blue", linetype = "dashed") +
  scale_fill_manual(values = c("TRUE" = "coral", "FALSE" = "steelblue")) +
  labs(title = "Club Enrollment",
       subtitle = "Red line = minimum (10), Blue line = capacity (15)",
       x = "Club",
       y = "Number of Students") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6))

ggsave("plot_club_enrollment.png", p2, width = 14, height = 6)
cat("Saved: plot_club_enrollment.png\n")

# 3. Clubs per Student
p3 <- ggplot(student_summary, aes(x = num_clubs)) +
  geom_histogram(binwidth = 1, fill = "forestgreen", color = "black", alpha = 0.7) +
  labs(title = "Number of Clubs per Student",
       x = "Number of Clubs",
       y = "Number of Students") +
  theme_minimal()

ggsave("plot_clubs_per_student.png", p3, width = 10, height = 6)
cat("Saved: plot_clubs_per_student.png\n")

# 4. Enrollment vs Club Rank (how popular were the clubs by avg rank)
# First check what columns assignments has
if (!"student_rank" %in% colnames(assignments)) {
  # Need to add rankings from responses
  responses_long <- responses %>%
    pivot_longer(cols = -1, names_to = "club_id", values_to = "student_rank") %>%
    mutate(club_id = trimws(tolower(club_id)))
  
  assignments <- assignments %>%
    left_join(responses_long %>% select(1, club_id, student_rank), 
              by = c(student_col, "club_id"))
}

club_popularity <- assignments %>%
  group_by(club_id, club_name) %>%
  summarise(
    avg_rank = mean(student_rank, na.rm = TRUE),
    students_count = n(),
    .groups = 'drop'
  ) %>%
  left_join(clubs %>% select(club_id, hours), by = "club_id")

p4 <- ggplot(club_popularity, aes(x = avg_rank, y = students_count)) +
  geom_point(aes(color = students_count < 10, shape = factor(hours), size = hours), alpha = 0.7) +
  geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 15, linetype = "dashed", color = "blue") +
  scale_color_manual(values = c("TRUE" = "coral", "FALSE" = "steelblue"),
                     name = "Under-enrolled") +
  scale_shape_manual(values = c("2" = 16, "4" = 17), name = "Hours/Week") +
  scale_size_continuous(range = c(3, 6), name = "Hours/Week") +
  labs(title = "Club Popularity vs Enrollment",
       subtitle = "Lower rank = more popular. Shape/size shows club hours",
       x = "Average Student Rank",
       y = "Number of Students Enrolled") +
  theme_minimal()

ggsave("plot_popularity_vs_enrollment.png", p4, width = 10, height = 6)
cat("Saved: plot_popularity_vs_enrollment.png\n")

# 5. Hour distribution by club
p5 <- ggplot(student_summary, aes(x = hours_assigned)) +
  geom_histogram(binwidth = 2, fill = "purple", color = "black", alpha = 0.6) +
  geom_vline(xintercept = 10, color = "red", linetype = "dashed", linewidth = 1.2) +
  annotate("text", x = 10, y = Inf, label = "Target: 10 hours", 
           vjust = 1.5, hjust = -0.1, color = "red", size = 4) +
  labs(title = "Distribution of Total Hours per Student",
       x = "Total Hours",
       y = "Number of Students") +
  theme_minimal() +
  theme(text = element_text(size = 12))

ggsave("plot_hours_distribution.png", p5, width = 10, height = 6)
cat("Saved: plot_hours_distribution.png\n")

cat("\n=== All visualizations saved! ===\n")