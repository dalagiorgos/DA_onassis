# club_assignment_deferred_acceptance_v9.R

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

# --- Διαβάζουμε δεδομένα ---
responses <- read_csv(responses_file, show_col_types = FALSE)
clubs <- read_csv(clubs_file, show_col_types = FALSE)

# --- Καθαρισμός και εναρμόνιση Clubs ---
colnames(clubs) <- tolower(trimws(colnames(clubs)))
club_name_source <- if("club_name" %in% colnames(clubs)) "club_name" else "club_id"
if("clubname" %in% colnames(clubs) & !("club_name" %in% colnames(clubs))) {
  club_name_source <- "clubname"
}

if("hours_per_week" %in% colnames(clubs)) {
  clubs <- clubs |> rename(hours = hours_per_week)
} else if(!("hours" %in% colnames(clubs))) {
  hours_col_name <- setdiff(colnames(clubs), c("club_id", "club_name", "clubname"))[1]
  if (!is.na(hours_col_name)) {
    clubs <- clubs |> rename(hours = hours_col_name)
  }
}

clubs <- clubs |>
  mutate(
    club_id = trimws(tolower(club_id)),
    club_name = .data[[club_name_source]], 
    club_name = trimws(club_name),
    hours = as.integer(hours),
    hours = if_else(is.na(hours), 2L, hours) 
  ) |>
  select(club_id, club_name, hours)

# --- Καθαρισμός και εναρμόνιση Responses ---
student_col <- colnames(responses)[1]
responses <- responses |>
  mutate(across(all_of(student_col), as.character)) 

initial_club_cols <- trimws(tolower(colnames(responses)[-1]))
colnames(responses) <- c(student_col, initial_club_cols)

responses_long_master <- responses |>
  pivot_longer(cols = -all_of(student_col),
               names_to = "ClubID", values_to = "Rank") |>
  mutate(
    ClubID = trimws(tolower(ClubID)),
    Rank = as.integer(Rank)
  ) |>
  filter(!is.na(Rank))

# --- Ενεργές Καταστάσεις ---
student_col <- colnames(responses)[1]
# Πίνακας για την τελική ανάθεση μαθητών
student_assignments <- data.frame(StudentID = unique(responses[[student_col]]),
                                  AssignedClubID = NA_character_,
                                  AssignmentRank = NA_integer_,
                                  stringsAsFactors = FALSE)
colnames(student_assignments)[1] <- student_col

# Πίνακας για τη χωρητικότητα των Ομίλων
club_capacity <- clubs |>
  select(club_id) |>
  mutate(CapacityLeft = max_club_capacity)

# Λίστα όλων των Ομίλων
all_club_ids <- clubs$club_id

# Λίστα μαθητών που έχουν λάβει τουλάχιστον 10 ώρες
students_with_full_hours <- character(0)
final_assignments <- data.frame(StudentID = character(), ClubID = character(), ClubName = character(), stringsAsFactors = FALSE)
colnames(final_assignments)[1] <- student_col

round <- 1L
max_rounds <- 50 # Μέγιστος αριθμός γύρων DA

cat("--- Έναρξη Αλγορίθμου Deferred Acceptance (DA) ---\n")

repeat {
  
  cat(sprintf("\n*** Γύρος DA %d ***\n", round))
  
  # Μαθητές που χρειάζονται ακόμα ώρες (<=10) και δεν έχουν ήδη ανάθεση σε αυτόν τον γύρο
  proposing_students <- student_assignments |>
    filter(is.na(AssignedClubID)) |>
    pull(all_of(student_col))
  
  # Εάν δεν υπάρχουν άλλοι μαθητές για πρόταση, τερματίζουμε
  if(length(proposing_students) == 0 || round > max_rounds) {
    cat("\nΤερματισμός: Όλοι οι μαθητές έχουν ανάθεση ή εξαντλήθηκαν οι γύροι.\n")
    break
  }
  
  # --- 1. Οι Μαθητές Προτείνουν (με βάση την υψηλότερη τρέχουσα προτίμηση) ---
  
  # Βρίσκουμε την υψηλότερη προτίμηση (το ελάχιστο Rank) για κάθε ενεργό μαθητή
  proposals <- responses_long_master |>
    filter(get(student_col) %in% proposing_students) |>
    group_by(across(all_of(student_col))) |>
    filter(Rank == min(Rank, na.rm = TRUE)) |>
    ungroup() |>
    # Κρατάμε μόνο την πρώτη επιλογή σε περίπτωση ισοπαλίας
    group_by(across(all_of(student_col))) |>
    slice_head(n = 1) |>
    ungroup() |>
    select(all_of(student_col), ClubID, Rank) |>
    rename(StudentRank = Rank)
  
  # Έλεγχος: Αν δεν υπάρχουν πλέον προτάσεις (εξαντλήθηκαν οι προτιμήσεις), τερματίζουμε
  if(nrow(proposals) == 0) {
    cat("\nΤερματισμός: Εξαντλήθηκαν οι προτιμήσεις των μαθητών.\n")
    break
  }
  
  cat(sprintf("-> Υποβλήθηκαν %d προτάσεις από μαθητές.\n", nrow(proposals)))
  
  # --- 2. Οι Όμιλοι Εξετάζουν τις Προτάσεις (Προσωρινή Αποδοχή) ---
  
  # Ομαδοποιούμε τις προτάσεις ανά Όμιλο
  club_decisions <- proposals |>
    # Συγχωνεύουμε με τις Ώρες του Ομίλου για το filtering
    left_join(clubs, by = c("ClubID" = "club_id")) |>
    group_by(ClubID) |>
    # Ταξινόμηση κατά Rank (χαμηλότερο = καλύτερο) και τυχαία σειρά (για tie-breaking)
    arrange(StudentRank, sample(n())) |>
    # Οι Όμιλοι επιλέγουν προσωρινά τους καλύτερους μέχρι τη χωρητικότητα
    slice_head(n = max_club_capacity) |>
    ungroup()
  
  # --- 3. Καταγραφή Αποδοχών και Απορρίψεων ---
  
  accepted_students <- club_decisions |> pull(all_of(student_col))
  
  # Μαθητές που απορρίφθηκαν από τον Όμιλο
  rejected_proposals <- proposals |>
    filter(!get(student_col) %in% accepted_students)
  
  # Μαθητές που έκαναν πρόταση αλλά δεν επιλέχθηκαν (πρέπει να μετακινήσουν την ψήφο τους)
  students_to_move_rank <- rejected_proposals |> pull(all_of(student_col))
  
  # --- 4. Καταχώρηση Αναθέσεων και Ενημέρωση Ranks ---
  
  # Προσθέτουμε τις νέες αναθέσεις
  new_assignments_current_round <- club_decisions |>
    select(all_of(student_col), ClubID, club_name, StudentRank, hours) |>
    rename(AssignmentRank = StudentRank,
           ClubHours = hours,
           ClubName = club_name)
  
  # Ενημέρωση του πίνακα τελικών αναθέσεων
  # (Επειδή το DA εξασφαλίζει μοναδική ανάθεση σε κάθε γύρο, μπορούμε απλώς να κάνουμε bind_rows)
  # Θα χρησιμοποιήσουμε τον club_assignments για την τελική μορφή
  
  # Προσθέτουμε τις νέες αναθέσεις στον τελικό πίνακα
  final_assignments <- final_assignments |>
    bind_rows(new_assignments_current_round |>
               # left_join(clubs |> select(club_id, club_name, hours), by = c("ClubID" = "club_id")) |>
              #  rename(ClubHours = hours)
                select(all_of(student_col), ClubID, ClubName, ClubHours)
    )
  
  # --- Ενημέρωση Προσωρινών Αναθέσεων Μαθητών ---
  # Οι μαθητές που έγιναν δεκτοί στον τρέχοντα γύρο δεν πρέπει να κάνουν νέα πρόταση αύριο.
  
  # Πίνακας με τους μαθητές που πρέπει να "κλειδωθούν"
  students_locked_in_this_round <- new_assignments_current_round |>
    select(all_of(student_col), ClubID) |>
    rename(AssignedClubID = ClubID)
  
  # Ενημερώνουμε τον πίνακα student_assignments με την προσωρινή ClubID
  # (ΜΟΝΟ για όσους δεν έχουν ήδη DA_COMPLETE)
  student_assignments <- student_assignments |>
    left_join(students_locked_in_this_round, by = student_col) |>
    mutate(
      # Χρησιμοποιούμε την ClubID του τρέχοντος γύρου αν είναι παρούσα (δηλ. έγινε δεκτός)
      # και αν δεν έχει ήδη τελειώσει το DA
      AssignedClubID = ifelse(is.na(AssignedClubID.x) | AssignedClubID.x == AssignedClubID.y, 
                              AssignedClubID.y, AssignedClubID.x)
    ) |>
    # Καθαρισμός βοηθητικών στηλών
    select(-AssignedClubID.x, -AssignedClubID.y)
  
  cat(sprintf("-> Αποδέχτηκαν %d μαθητές συνολικά σε αυτόν τον γύρο.\n", nrow(new_assignments_current_round)))
  
  # Ενημέρωση των ωρών
  student_hours <- final_assignments |>
    group_by(across(all_of(student_col))) |>
    summarise(HoursAssigned = sum(ClubHours, na.rm = TRUE), .groups = 'drop') |>
    right_join(student_assignments |> select(all_of(student_col)), by = student_col) |>
    replace_na(list(HoursAssigned = 0L))
  
  # Βρίσκουμε τους μαθητές που συμπλήρωσαν τις 10 ώρες σε αυτόν τον γύρο
  students_now_full <- student_hours |>
    filter(HoursAssigned >= required_hours) |>
    pull(all_of(student_col))
  
  # Μαθητές που πρέπει να μετακινήσουν την ψήφο τους:
  # 1. Όσοι απορρίφθηκαν (students_to_move_rank)
  # 2. Όσοι συμπλήρωσαν τις 10 ώρες (students_now_full)
  
  students_to_move_rank <- union(students_to_move_rank, students_now_full)
  
  # Ακύρωση του Όμιλου/Ώρες για τους μαθητές που πρέπει να μετακινήσουν την ψήφο τους
  for(sid in students_to_move_rank) {
    
    # 1. Αν ο μαθητής απορρίφθηκε (students_to_move_rank), ακυρώνουμε την τρέχουσα προτίμηση του
    # 2. Αν ο μαθητής συμπλήρωσε τις ώρες (students_now_full), ακυρώνουμε ΟΛΕΣ τις προτιμήσεις του
    
    if (sid %in% students_now_full) {
      # Ακυρώνουμε ΟΛΕΣ τις προτιμήσεις του μαθητή
      responses_long_master <- responses_long_master |>
        mutate(Rank = ifelse(get(student_col) == sid, NA_integer_, Rank))
    } else {
      # Ακυρώνουμε ΜΟΝΟ την προτίμηση που απέρριψε
      club_id_rejected <- rejected_proposals |> filter(get(student_col) == sid) |> pull(ClubID)
      responses_long_master <- responses_long_master |>
        mutate(Rank = ifelse(get(student_col) == sid & ClubID == club_id_rejected, NA_integer_, Rank))
    }
  }
  
  # Αφαιρούμε τα NA και επανα-αριθμούμε τα Ranks
  responses_long_master <- responses_long_master |>
    filter(!is.na(Rank)) |>
    group_by(across(all_of(student_col))) |>
    mutate(Rank = rank(Rank, ties.method = "min")) |> 
    ungroup()
  
  # Ολοκληρωμένη η ανάθεση για τους μαθητές που συμπλήρωσαν 10 ώρες
  students_assignments_to_remove <- final_assignments |>
    filter(get(student_col) %in% students_now_full) |>
    pull(all_of(student_col))
  
  student_assignments <- student_assignments |>
    mutate(AssignedClubID = ifelse(get(student_col) %in% students_now_full, "DA_COMPLETE", AssignedClubID))
  
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
write_csv(final_assignments |> select(all_of(student_col), ClubID, ClubName), "club_assignments_DA_final.csv")
write_csv(final_student_hours, "student_hours_DA_log.csv")
write_csv(student_club_list, "student_club_list_DA.csv")
write_csv(clubs_less_than_10, "clubs_less_than_10_students_DA.csv")

cat("\n\n--- ΑΠΟΤΕΛΕΣΜΑΤΑ DEFERRED ACCEPTANCE (DA) ---\n")
cat("Τελικές αναθέσεις: club_assignments_DA_final.csv\n")
cat("Ώρες ανά μαθητή: student_hours_DA_log.csv\n")
cat("Λίστα Ομίλων ανά μαθητή: student_club_list_DA.csv\n")
cat("\n--- ΟΜΙΛΟΙ ΜΕ ΛΙΓΟΤΕΡΟΥΣ ΑΠΟ 10 ΜΑΘΗΤΕΣ ---\n")
print(clubs_less_than_10)