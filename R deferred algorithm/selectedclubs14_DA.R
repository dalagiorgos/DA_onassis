# club_assignment_deferred_acceptance_v14_final_fix.R

library(dplyr)
library(readr)
library(tidyr)
library(stringr)

# --- Ρυθμίσεις ---
max_club_capacity <- 15L # Μέγιστη χωρητικότητα Ομίλου (για το DA)
required_hours <- 10L
responses_file <- "responses.csv" 
clubs_file <- "clubs.csv"
set.seed(42) # Για τυχαία επιλογή σε περίπτωση ισοπαλίας

# --- 1. Διαβάζουμε και Καθαρίζουμε Δεδομένα ---
responses <- read_csv(responses_file, show_col_types = FALSE)
clubs <- read_csv(clubs_file, show_col_types = FALSE)

# Καθαρισμός και εναρμόνιση Clubs
colnames(clubs) <- tolower(trimws(colnames(clubs)))
club_name_source <- if("club_name" %in% colnames(clubs)) "club_name" else "club_id"
if("clubname" %in% colnames(clubs) & !("club_name" %in% colnames(clubs))) {
  club_name_source <- "clubname"
}

if("hours_per_week" %in% colnames(clubs)) {
  clubs <- clubs |> rename(hours = hours_per_week)
} else if(!("hours" %in% colnames(clubs))) {
  # Αν δεν βρεθεί hours_per_week ή hours, υποθέτουμε 2 ώρες/εβδομάδα
  clubs <- clubs |> mutate(hours = 2L)
}

student_col <- colnames(responses)[1]

# Καθαρισμός Responses
active_clubs <- trimws(tolower(colnames(responses)[-1]))
colnames(responses) <- c(student_col, active_clubs)

# --- Μετατροπή Λίστας Προτίμησης σε Long Format (Βήμα 1) ---
responses_long_master <- responses |>
  pivot_longer(cols = -all_of(student_col),
               names_to = "ClubID", values_to = "Rank") |>
  mutate(
    ClubID = trimws(tolower(ClubID)),
    ClubID = as.character(ClubID), 
    StudentRank = as.integer(Rank) # Διατηρούμε το Rank του μαθητή
  ) |>
  filter(!is.na(StudentRank)) |> # Κρατάμε μόνο τις προτιμήσεις
  # Προετοιμασία για το DA: το Rank θα αλλάζει, το StudentRank είναι το αρχικό
  group_by(across(all_of(student_col))) |>
  mutate(Rank = rank(StudentRank, ties.method = "min")) |> 
  ungroup()


# --- Αρχικοποίηση ---
final_assignments <- tibble(
  !!student_col := character(0),
  ClubID = character(0),
  ClubName = character(0),
  ClubHours = integer(0)
)

student_assignments <- responses |> 
  select(all_of(student_col)) |>
  # AssignedClubID: Χρησιμοποιείται μόνο για "DA_COMPLETE" όταν φτάσει τις 10 ώρες.
  mutate(
    AssignedClubID = NA_character_, 
    HoursAssigned = 0L,
    CurrentRank = 1L # Ξεκινάμε με την καλύτερη επιλογή (Rank 1)
  )

round <- 1L
max_rounds <- length(unique(responses_long_master$ClubID)) * 2 # Αρκετοί γύροι

cat(sprintf("Έναρξη αλγορίθμου Deferred Acceptance. Απαιτούμενες ώρες: %d. Μέγιστη χωρητικότητα Ομίλου: %d\n", 
            required_hours, max_club_capacity))

# --- Κεντρικός Αλγόριθμος DA ---

while (round <= max_rounds) {
  
  # Μαθητές που χρειάζονται ακόμα ώρες και έχουν ενεργές προτιμήσεις
  proposing_students_data <- student_assignments |>
    filter(AssignedClubID != "DA_COMPLETE") |>
    inner_join(responses_long_master, by = student_col) |>
    group_by(across(all_of(student_col))) |>
    filter(Rank == min(Rank)) |>
    slice_sample(n = 1) |> # Πρόταση μόνο στον καλύτερο διαθέσιμο
    ungroup() |>
    select(all_of(student_col), ClubID, StudentRank)
  
  if (nrow(proposing_students_data) == 0) {
    cat(sprintf("Τερματισμός. Δεν υπάρχουν ενεργοί μαθητές/προτάσεις στον Γύρο %d.\n", round))
    break
  }
  
  cat(sprintf("--- Γύρος %d: %d προτάσεις. ---\n", round, nrow(proposing_students_data)))
  
  # --- 2. Πρόταση: Οι μαθητές προτείνουν στον καλύτερο διαθέσιμο όμιλο ---
  proposals <- proposing_students_data |>
    # Η στήλη StudentRank είναι το ranking του μαθητή για τον Όμιλο (χρησιμοποιείται για το club ranking)
    rename(StudentRank = StudentRank) 
  
  # --- 3. Απόφαση Ομίλου: Οι Όμιλοι επιλέγουν/απορρίπτουν ---
  club_decisions <- proposals |>
    left_join(clubs, by = c("ClubID" = "club_id")) |> # ΣΥΝΔΕΣΗ ΜΕ club_name ΚΑΙ hours
    group_by(ClubID) |>
    # Ranking των μαθητών από τον Όμιλο: Προτιμά τον μαθητή που του έδωσε καλύτερο Rank (μικρότερο StudentRank)
    arrange(StudentRank, sample(n())) |> 
    # Οι Όμιλοι επιλέγουν προσωρινά τους καλύτερους μέχρι τη χωρητικότητα
    slice_head(n = max_club_capacity) |> 
    ungroup()
  
  # Ποιοι μαθητές απορρίφθηκαν
  rejected_proposals <- anti_join(proposals, club_decisions, by = c(student_col, "ClubID"))
  
  # --- 4. Καταχώρηση Αναθέσεων και Ενημέρωση Ranks ---
  
  # Νέες αναθέσεις για τον τρέχοντα γύρο
  new_assignments_current_round <- club_decisions |>
    # ΔΙΟΡΘΩΣΗ 1: Χρησιμοποιούμε club_name και μετονομάζουμε σε ClubName (Λογική v12)
    select(all_of(student_col), ClubID, club_name, StudentRank, hours) |> 
    rename(
      AssignmentRank = StudentRank, 
      ClubHours = hours,
      ClubName = club_name 
    )
  
  # Προσθέτουμε τις νέες αναθέσεις στον τελικό πίνακα
  final_assignments <- final_assignments |>
    bind_rows(new_assignments_current_round |>
                select(all_of(student_col), ClubID, ClubName, ClubHours)
    )
  
  cat(sprintf("-> Αποδέχτηκαν %d μαθητές συνολικά σε αυτόν τον γύρο.\n", nrow(new_assignments_current_round)))
  
  # --- Ενημέρωση Ωρών και Μαθητών με Πλήρη Ανάθεση ---
  student_hours <- final_assignments |>
    group_by(across(all_of(student_col))) |>
    summarise(HoursAssigned = sum(ClubHours, na.rm = TRUE), .groups = 'drop') |>
    right_join(student_assignments |> select(all_of(student_col)), by = student_col) |>
    replace_na(list(HoursAssigned = 0L))
  
  # Βρίσκουμε τους μαθητές που συμπλήρωσαν τις 10 ώρες σε αυτόν τον γύρο
  students_now_full <- student_hours |>
    filter(HoursAssigned >= required_hours) |>
    pull(all_of(student_col))
  
  # --- 5. Ενημέρωση Προτιμήσεων Μαθητών (ΔΙΟΡΘΩΣΗ 2: Λογική v13) ---
  
  # 5α. Διαγραφή προτιμήσεων Ομίλων που απέρριψαν μαθητές
  if (nrow(rejected_proposals) > 0) {
    for (i in 1:nrow(rejected_proposals)) {
      sid <- rejected_proposals[i, 1] |> pull()
      club_id_rejected <- rejected_proposals$ClubID[i]
      
      # Διαγραφή μόνο της συγκεκριμένης απορριφθείσας ClubID
      responses_long_master <- responses_long_master |>
        filter(!(get(student_col) == sid & ClubID == club_id_rejected))
    }
  }
  
  # 5β. Διαγραφή προτιμήσεων Ομίλων που δέχτηκαν προσωρινά μαθητές (ΚΡΙΣΙΜΟ)
  # Ο μαθητής πρέπει να διαγράψει την επιλογή που τον δέχτηκε για να προχωρήσει στην επόμενη.
  if (nrow(new_assignments_current_round) > 0) {
    for (i in 1:nrow(new_assignments_current_round)) {
      sid <- new_assignments_current_round[i, 1] |> pull()
      club_id_accepted <- new_assignments_current_round$ClubID[i]
      
      # Διαγραφή μόνο της συγκεκριμένης αποδεχθείσας ClubID
      responses_long_master <- responses_long_master |>
        filter(!(get(student_col) == sid & ClubID == club_id_accepted))
    }
  }
  
  # 5γ. Ακύρωση ΟΛΩΝ των προτιμήσεων για μαθητές που έχουν συμπληρώσει 10 ώρες
  if (length(students_now_full) > 0) {
    responses_long_master <- responses_long_master |>
      filter(!get(student_col) %in% students_now_full)
  }
  
  # 5δ. Επανα-αρίθμηση των Ranks (βάσει του αρχικού StudentRank)
  responses_long_master <- responses_long_master |>
    group_by(across(all_of(student_col))) |>
    # Το rank γίνεται τώρα στο StudentRank, αλλά μόνο για τις ενεργές επιλογές
    mutate(Rank = rank(StudentRank, ties.method = "min")) |> 
    ungroup()
  
  # 5ε. Ενημέρωση της λίστας ενεργών μαθητών:
  student_assignments <- student_assignments |>
    left_join(student_hours |> select(all_of(student_col), HoursAssigned), by = student_col) |>
    # Ολοκληρωμένη η ανάθεση για τους μαθητές που συμπλήρωσαν 10 ώρες
    mutate(AssignedClubID = ifelse(get(student_col) %in% students_now_full, "DA_COMPLETE", NA_character_)) |>
    select(-HoursAssigned) # Καθαρίζουμε την βοηθητική στήλη
  
  round <- round + 1L
}

# --- Τελική Καταγραφή & Εκτύπωση ---

final_student_hours <- final_assignments |>
  group_by(across(all_of(student_col))) |>
  summarise(HoursAssigned = sum(ClubHours, na.rm = TRUE), .groups = 'drop')

# Δημιουργία λίστας Ομίλων ανά μαθητή
student_club_list <- final_assignments |>
  group_by(across(all_of(student_col))) |>
  summarise(
    Clubs_Assigned = paste(ClubName, collapse = ", "),
    .groups = 'drop'
  ) |>
  left_join(final_student_hours, by = student_col) |>
  select(all_of(student_col), Clubs_Assigned, HoursAssigned)

# Υπολογισμός Ομίλων με < 10 μαθητές
clubs_less_than_10 <- final_assignments |>
  group_by(ClubName) |>
  summarise(
    Students_Count = n(),
    .groups = 'drop'
  ) |>
  filter(Students_Count < 10) |>
  arrange(Students_Count, ClubName)

# --- Αποθήκευση Τελικών Αρχείων ---
write_csv(final_assignments |> select(all_of(student_col), ClubID, ClubName, ClubHours), "club_assignments_DA_final.csv")
write_csv(student_club_list, "student_club_list_DA.csv")
write_csv(clubs_less_than_10, "clubs_less_than_10_students_DA.csv")
write_csv(final_student_hours, "student_hours_DA_log.csv")

cat("\nΔιαδικασία ολοκληρώθηκε. Τα τελικά αρχεία έχουν αποθηκευτεί.\n")