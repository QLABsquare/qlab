# _______________________________________________________________________________________

# (0) install necessary packages / set working directory
# _______________________________________________________________________________________
install.packages(c("ggtext", "psycho", "gt", "patchwork", "webshot2", "httr", "jsonlite", "rlang", "tidyverse", "corrr", "ggplot2", "purrr", "gridExtra", "psych", "gtable", "mediation", "ppcor", "lme4", "lmerTest", "httr", "googleAuthR", "webshot", "knitr", "kableExtra", "reshape", "data.table"))
library(ggtext);library(psycho);library(patchwork); library(gt); library(webshot2); library(httr); library(jsonlite); library(rlang); library(tidyverse); library(corrr); library(ggplot2); library(purrr); library(gridExtra); library(psych); library(gtable); library(mediation); library(ppcor); library(lme4); library(lmerTest); library(httr); library(googleAuthR); library(webshot); library(knitr); library(kableExtra); library(reshape); library(data.table)

# integration with github 
setwd('/home/b.siles/wd_manuscript')


# _______________________________________________________________________________________

# (1) read in necessary files
# _______________________________________________________________________________________

# gorilla outputs for ssl and lsl; includes 2afc and lsl sicr
gorilla_ssl_lsl_output_cohort1 <- list.files('~/wd_manuscript/data/cohort_1_gorilla_outputs', full.names = TRUE)
gorilla_ssl_lsl_output_cohort2 <- list.files('~/wd_manuscript/data/cohort_2_gorilla_outputs', full.names = TRUE)

# ssl sicr transcriptions
# no s1 sicr data; already in long, unscored format:
ssl_sicr_transcript_cohort1_s2 <- read.csv('~/wd_manuscript/data/wd_cohort1_ssl_s2_transcript_03.12.2025.csv')
# need to be transformed into long, unscored format:
ssl_sicr_transcript_cohort2_s1 <- read.csv('~/wd_manuscript/data/wd_cohort2_ssl_s1_transcript_03.12.2025.csv')
ssl_sicr_transcript_cohort2_s2 <- read.csv('~/wd_manuscript/data/wd_cohort2_ssl_s2_transcript_03.12.2025.csv')

View(ssl_sicr_transcript_cohort1_s2)

# assessment files
wd_assessments_cohort1 <- read.csv('~/wd_manuscript/data/wd_cohort1_assessments.csv')
wd_assessments_cohort2 <- read.csv('~/wd_manuscript/data/wd_cohort2_assessments.csv')

# demographic files
wd_demographics_cohort1 <- read.csv('~/wd_manuscript/data/wd_cohort1_demographics.csv')%>%
  filter(record_id != '7')

wd_demographics_cohort2 <- read.csv('~/wd_manuscript/data/wd_cohort2_demographics.csv')

View(wd_assessments_cohort2)
View(wd_demographics_cohort1)
# _______________________________________________________________________________________

# (2) data processing, scoring, formatting, and analysis functions 
# _______________________________________________________________________________________

# ________ SSL SICR scoring functions ________

# converts SSL gorilla outputs to individual answer keys
generate_ssl_answers <- function(csv_files) {
  
  # combine all input files into one dataframe
  ssl_raw_files <- csv_files %>%
    lapply(read.csv) %>%
    bind_rows()
  
  # Generate answer keys
  ssl_key <- ssl_raw_files %>%
    dplyr::select(Participant.Public.ID, Display, Trial.Number) %>%
    filter(Display == 'SICR') %>%
    distinct() %>%
    mutate(answer_key = case_when(
      Trial.Number == 1 ~ 'da/ro/pi/go/la/tu',
      Trial.Number == 2 ~ 'pa/bi/ku/da/ro/pi',
      Trial.Number == 3 ~ 'go/tu/ro/pi/la/da',
      Trial.Number == 4 ~ 'tu/ro/la/da/pi/go',
      Trial.Number == 5 ~ 'bu/da/pi/ti/ro/do',
      Trial.Number == 6 ~ 'la/bi/go/ku/tu/pa',
      Trial.Number == 7 ~ 'do/pa/ti/bu/ku/bi',
      Trial.Number == 8 ~ 'pa/bi/ku/go/la/tu',
      Trial.Number == 9 ~ 'tu/bu/do/pa/bi/ku',
      Trial.Number == 10 ~ 'bu/go/la/do/ti/tu',
      Trial.Number == 11 ~ 'da/ro/pi/ti/bu/do',
      Trial.Number == 12 ~ 'pa/bi/ku/ti/bu/do',
      Trial.Number == 13 ~ 'go/la/tu/ti/bu/do',
      Trial.Number == 14 ~ 'do/bu/ku/bi/pa/ti',
      Trial.Number == 15 ~ 'go/la/tu/da/ro/pi',
      Trial.Number == 16 ~ 'da/bi/pa/ku/pi/ro'
    )) %>%
    dplyr::select(Participant.Public.ID, Trial.Number, answer_key)
  
  # Reshape to wide format
  ssl_answer_key <- reshape(ssl_key, idvar = "Participant.Public.ID", timevar = "Trial.Number", direction = "wide")
  
  return(ssl_answer_key)
}

# converts raw ssl transcriptions into long format for scoring: 'id, trial number, trial type, response, answer key'
ssl_unscored <- function(ssl_raw_transcriptions) {
  
  ssl_long_format <- ssl_raw_transcriptions %>%
    # filter(!startsWith(id, "pl")) %>%
    mutate(id = case_when(
      id == 'pilot_xx' ~ 'pilot_51',
      TRUE ~ id)) %>%
    pivot_longer(
      cols = starts_with("X"),   
      names_to = "trial_number",         
      values_to = "Value"                 
    ) %>%
    pivot_wider(
      names_from = type,                   
      values_from = Value
    ) %>%
    # Clean up the Trial_Number column to remove the "X" prefix and convert to numeric
    mutate(trial_number = as.numeric(gsub("X", "", trial_number))) %>%
    mutate(target_tag = case_when(
      key == 'da/ro/pi/go/la/tu' ~ 1,
      key == 'pa/bi/ku/da/ro/pi' ~ 1,
      key == 'go/tu/ro/pi/la/da' ~ 0,
      key == 'tu/ro/la/da/pi/go' ~ 0,
      key == 'bu/da/pi/ti/ro/do' ~ 0,
      key == 'la/bi/go/ku/tu/pa' ~ 0,
      key == 'do/pa/ti/bu/ku/bi' ~ 0,
      key == 'pa/bi/ku/go/la/tu' ~ 1,
      key == 'ti/bu/do/pa/bi/ku' ~ 1,
      key == 'tu/bu/do/pa/bi/ku' ~ 1,
      key == 'bu/go/la/do/ti/tu' ~ 0,
      key == 'da/ro/pi/ti/bu/do' ~ 1,
      key == 'pa/bi/ku/ti/bu/do' ~ 1,
      key == 'go/la/tu/ti/bu/do' ~ 1,
      key == 'do/bu/ku/bi/pa/ti' ~ 0,
      key == 'go/la/tu/da/ro/pi' ~ 1,
      key == 'da/bi/pa/ku/pi/ro' ~ 0
    )) %>%
    dplyr::select(id, trial_number, target_tag, response, key)
  
  return(ssl_long_format)
}

# scores individual responses in the ssl dataframe; returns the scored version (trial-by-trial)
scorer <- function(response, key) {
  # Split the response and key into syllables
  response_syllables <- str_split(response, "/")[[1]]
  key_syllables <- str_split(key, "/")[[1]]
  
  # Score correct serial syllables (exact match)
  correct_serial_syllables <- sum(response_syllables == key_syllables)
  
  # Generate all bigrams from response
  response_bigrams <- paste0(head(response_syllables, -1), "/", tail(response_syllables, -1))
  
  # Generate only first two and last two bigrams from key
  n_key <- length(key_syllables)
  if (n_key >= 3) {
    # First two bigrams
    key_first_bigrams <- paste0(key_syllables[1:2], "/", key_syllables[2:3])
    
    # Last two bigrams
    key_last_bigrams <- paste0(key_syllables[(n_key-2):(n_key-1)], "/", key_syllables[(n_key-1):n_key])
    
    # Combine first and last bigrams for key
    key_bigrams <- c(key_first_bigrams, key_last_bigrams)
    
    # Score correct bigrams - count each key bigram only once even if it appears multiple times in response
    # For each key bigram, check if it appears in response_bigrams at all (max score of 1 per key bigram)
    correct_bigrams <- sum(sapply(key_bigrams, function(kb) any(response_bigrams == kb)))
  } else {
    # Handle case where key has fewer than 3 syllables
    correct_bigrams <- 0
  }
  
  # Score correct trigrams
  response_trigrams <- paste0(head(response_syllables, -2), "/", 
                              head(tail(response_syllables, -1), -1), "/", 
                              tail(response_syllables, -2))
  
  # Generate only first and last trigrams from key
  if (n_key >= 3) {
    key_first_trigram <- paste0(key_syllables[1], "/", key_syllables[2], "/", key_syllables[3])
    key_last_trigram  <- paste0(key_syllables[n_key - 2], "/", key_syllables[n_key - 1], "/", key_syllables[n_key])
    
    key_trigrams <- c(key_first_trigram, key_last_trigram)
    
    correct_trigrams <- sum(sapply(key_trigrams, function(kt) any(response_trigrams == kt)))
  } else {
    correct_trigrams <- 0
  }
  
  # Score correct total syllables (order doesn't matter)
  correct_total_syllables <- sum(response_syllables %in% key_syllables)
  
  return(data.frame(correct_serial_syllables = correct_serial_syllables,
                    correct_bigrams = correct_bigrams,
                    correct_trigrams = correct_trigrams,
                    correct_total_syllables = correct_total_syllables))
}
ssl_scored <- function(df) {
  
  # Get the name of the input dataframe
  df_name <- deparse(substitute(df))
  print(df_name)
  
  # Extract the last character of the dataframe name
  session_num <- substr(df_name, nchar(df_name), nchar(df_name))
  
  # Function to generate valid permutations of the response by adding "xx"
  generate_permutations <- function(response_syllables) {
    # If the response is already 6 syllables, no modification is needed
    if (length(response_syllables) == 6) {
      return(list(response_syllables))
    }
    
    # Check if the response has fewer than 6 syllables
    if (length(response_syllables) < 6) {
      # Calculate how many "xx" syllables to add
      xx_needed <- 6 - length(response_syllables)
      
      # Generate all positions where "xx" can be inserted
      positions <- 1:(length(response_syllables) + xx_needed)
      possible_permutations <- list()
      
      # Create all valid permutations of "xx" and insert them at the correct positions
      combinations <- combn(positions, xx_needed, simplify = FALSE)
      for (comb in combinations) {
        temp_response <- response_syllables
        for (pos in comb) {
          temp_response <- append(temp_response, "xx", after = pos - 1)
        }
        possible_permutations[[length(possible_permutations) + 1]] <- temp_response
      }
      
      return(possible_permutations)
    } else {
      # If response has more than 6 syllables, trim it to 6
      return(list(response_syllables[1:6]))
    }
  }
  
  # Initialize an empty list to store the scores for each response
  scores_list <- list()
  
  # Process each row of the dataframe
  for (i in 1:nrow(df)) {
    response_syllables <- str_split(df$response[i], "/")[[1]]
    permutations <- generate_permutations(response_syllables)
    
    # Initialize a vector to store the scores for each permutation
    permutation_scores <- data.frame(correct_serial_syllables = numeric(length(permutations)),
                                     correct_bigrams = numeric(length(permutations)),
                                     correct_trigrams = numeric(length(permutations)),
                                     correct_total_syllables = numeric(length(permutations)),
                                     permutation_used = character(length(permutations)))
    
    # Calculate scores for each permutation
    for (j in 1:length(permutations)) {
      scored <- scorer(paste(permutations[[j]], collapse = "/"), df$key[i])
      permutation_scores[j, 1:4] <- scored
      permutation_scores$permutation_used[j] <- paste(permutations[[j]], collapse = "/")  # Store the permutation used
    }
    
    # Find the permutation with the highest syllable score
    best_permutation_index <- which.max(permutation_scores$correct_serial_syllables)
    
    # Store the best scores and permutation string for the current row
    scores_list[[i]] <- c(permutation_scores[best_permutation_index, 1:4], 
                          permutation_used = permutation_scores$permutation_used[best_permutation_index])
  }
  
  # Convert the list of scores to a dataframe
  scored_df <- bind_rows(scores_list) %>%
    mutate(session_number = session_num)
  
  # Determine which columns to keep from the original dataframe
  columns_to_keep <- c("id", "trial_number", "target_tag", "response", "key")
  
  # Add the relevant columns from the original dataframe (id, trial_number, etc.)
  final_df <- cbind(df[, c("id", "trial_number", "target_tag", "response", "key")], scored_df)
  
  return(final_df)
}

# converts trial-by-trial version to long version of participant average scores, each participant has two rows: one for target trials, and one for foil trials
ssl_long_mean_scored <- function(df) {
  # First pass: Calculate num_responses.ssl for each id and target_tag
  ssl_long_mean_scored_df <- df %>%
    mutate(id = as.character(as.numeric(gsub("\\D", "", id)))) %>%
    dplyr::select(-permutation_used) %>%
    group_by(id, target_tag) %>%
    mutate(num_responses.ssl = sum((response != "") & (!is.na(response)) & (response != 'N/A')))
  
  # Identify IDs where both target_tags have > 3 responses
  valid_ids <- ssl_long_mean_scored_df %>%
    group_by(id, target_tag) %>%
    summarize(has_enough = min(num_responses.ssl) > 2, .groups = "drop") %>%
    group_by(id) %>%
    summarize(all_valid = all(has_enough) && n() == 2, .groups = "drop") %>%
    filter(all_valid) %>%
    pull(id)
  
  # Second pass: Filter to only include the valid IDs and calculate metrics
  ssl_long_mean_scored_df <- ssl_long_mean_scored_df %>%
    filter(id %in% valid_ids) %>%
    group_by(id, target_tag) %>%
    mutate(correct_serial_syllables.ssl = sum(correct_serial_syllables)/(num_responses.ssl*6)) %>%
    mutate(correct_syllables_total.ssl = sum(correct_total_syllables)/(num_responses.ssl*6)) %>%
    mutate(correct_bigrams.ssl = sum(correct_bigrams)/(num_responses.ssl*4)) %>%
    mutate(correct_trigrams.ssl = sum(correct_trigrams)/(num_responses.ssl*2)) %>%
    dplyr::select(id, target_tag, num_responses.ssl, correct_serial_syllables.ssl, correct_bigrams.ssl, correct_trigrams.ssl, correct_syllables_total.ssl, session_number) %>%
    distinct() %>%
    mutate(target_tag = as.factor(target_tag))
  
  return(ssl_long_mean_scored_df)
}

# converts long version of participant average scores to wide; adds in difference scores: (target - foil) / (target + foil)
ssl_wide_mean_scored <- function(df){
  
  # First create the wide format dataframe as before
  ssl_wide_mean_scored_df <- df %>%
    pivot_wider(names_from = target_tag, 
                values_from = where(is.numeric)) %>%
    dplyr::select(id, correct_serial_syllables.ssl_0, correct_serial_syllables.ssl_1, 
                  correct_bigrams.ssl_0, correct_bigrams.ssl_1, 
                  correct_trigrams.ssl_0, correct_trigrams.ssl_1, 
                  correct_syllables_total.ssl_0, correct_syllables_total.ssl_1, 
                  session_number) %>%
    mutate(overall_diff_syl.ssl = (correct_serial_syllables.ssl_1 - correct_serial_syllables.ssl_0)/
             (correct_serial_syllables.ssl_1 + correct_serial_syllables.ssl_0),
           overall_diff_syllable.ssl = (correct_bigrams.ssl_1 - correct_bigrams.ssl_0)/
             (correct_bigrams.ssl_1 + correct_bigrams.ssl_0))
  
  # Get unique session number value (assuming there's only one per dataframe)
  session_num <- unique(ssl_wide_mean_scored_df$session_number)
  
  # Create a suffix based on the session number
  suffix <- paste0("_s", session_num)
  
  # Get all column names except 'id' and 'session_number'
  cols_to_rename <- setdiff(names(ssl_wide_mean_scored_df), c("id", "session_number"))
  
  # Create a named vector for renaming
  new_names <- setNames(paste0(cols_to_rename, suffix), cols_to_rename)
  
  # Rename the columns
  ssl_wide_mean_scored_df <- ssl_wide_mean_scored_df %>%
    rename_with(~ new_names[.x], .cols = all_of(cols_to_rename)) %>%
    dplyr::select(-session_number)
  
  return(ssl_wide_mean_scored_df)
}

# ________ SSL 2AFC scoring functions ________

# calculates 2afc accuracy for both cohorts; creates & saves group-level plots
cohort2_afc_accuracy <- function(csv_files) {
  # combine all input files into one dataframe
  ssl_raw_files <- csv_files %>%
    lapply(read.csv) %>%
    bind_rows()
  
  ssl_filtered <- ssl_raw_files %>%
    dplyr::select(Participant.Public.ID, Task.Name, Trial.Number, Screen, Response.Type, Response, 
                  Spreadsheet..audio, Spreadsheet..audio2, Spreadsheet..audio3, 
                  Spreadsheet..audio4, Spreadsheet..audio5, Spreadsheet..audio6,
                  Spreadsheet..sicr1, Spreadsheet..sicr2, Spreadsheet..sicr3,
                  Spreadsheet..sicr4, Spreadsheet..sicr5, Spreadsheet..sicr6) %>%
    mutate(
      Participant.Public.ID = case_when(
        Participant.Public.ID == 'naomi' ~ 'wd_25',
        Participant.Public.ID == 'wd _13_s2' ~ 'wd_13_s2',
        TRUE ~ Participant.Public.ID
      ),
      task = case_when(
        grepl("SSL", Task.Name) ~ "ssl",
        grepl("LSL", Task.Name) ~ "lsl",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(Participant.Public.ID != 'prad' & Participant.Public.ID != "brynnn" & 
             Participant.Public.ID != "" & !is.na(Participant.Public.ID)) %>%
    arrange(Participant.Public.ID, Trial.Number) %>%  # Ensure correct order
    group_by(Participant.Public.ID, Trial.Number) %>% 
    mutate(
      prev_sicr1 = lag(Spreadsheet..sicr1),
      prev_sicr2 = lag(Spreadsheet..sicr2),
      prev_sicr3 = lag(Spreadsheet..sicr3),
      prev_sicr4 = lag(Spreadsheet..sicr4),
      prev_sicr5 = lag(Spreadsheet..sicr5),
      prev_sicr6 = lag(Spreadsheet..sicr6)
    ) %>%
    ungroup() %>%
    filter(Screen == '2afc choices' | (Screen == '2afc' & Response.Type == 'response')) %>%
    mutate(
      option1 = case_when(
        task == "ssl" ~ paste0(
          substr(Spreadsheet..audio, 1, 2),
          substr(Spreadsheet..audio2, 1, 2),
          substr(Spreadsheet..audio3, 1, 2)
        ),
        task == "lsl" & Response.Type == "response" ~ paste0(
          substr(prev_sicr1, 1, 1),
          substr(prev_sicr2, 1, 1),
          substr(prev_sicr3, 1, 1)
        ),
        task == "lsl" ~ paste0(
          substr(Spreadsheet..sicr1, 1, 1),
          substr(Spreadsheet..sicr2, 1, 1),
          substr(Spreadsheet..sicr3, 1, 1)
        ),
        TRUE ~ NA_character_
      ),
      option2 = case_when(
        task == "ssl" ~ paste0(
          substr(Spreadsheet..audio4, 1, 2),
          substr(Spreadsheet..audio5, 1, 2),
          substr(Spreadsheet..audio6, 1, 2)
        ),
        task == "lsl" & Response.Type == "response" ~ paste0(
          substr(prev_sicr4, 1, 1),
          substr(prev_sicr5, 1, 1),
          substr(prev_sicr6, 1, 1)
        ),
        task == "lsl" ~ paste0(
          substr(Spreadsheet..sicr4, 1, 1),
          substr(Spreadsheet..sicr5, 1, 1),
          substr(Spreadsheet..sicr6, 1, 1)
        ),
        TRUE ~ NA_character_
      ),
      Response = substr(Response, nchar(Response), nchar(Response))
    ) %>%
    filter(Response != 'd' & Response != '' & option1 != '') %>%
    dplyr::select(Participant.Public.ID, task, Trial.Number, Response, option1, option2) %>%
    mutate(key = case_when(
      option1 == 'pabiku' ~ 1,
      option1 == 'daropi' ~ 1,
      option1 == 'golatu' ~ 1,
      option1 == 'tibudo' ~ 1,
      option2 == 'pabiku' ~ 2,
      option2 == 'daropi' ~ 2,
      option2 == 'golatu' ~ 2,
      option2 == 'tibudo' ~ 2,
      
      option1 == 'GJA' ~ 1,
      option1 == 'FKC' ~ 1,
      option1 == 'LBE' ~ 1,
      option1 == 'MDH' ~ 1,
      option2 == 'GJA' ~ 2,
      option2 == 'FKC' ~ 2,
      option2 == 'LBE' ~ 2,
      option2 == 'MDH' ~ 2,
      TRUE ~ 3
    )) %>%
    filter(key != 3) %>%
    distinct()
  
  # Create a new column for original ID before we modify it (only for filtering purposes)
  ssl_filtered$Original.ID <- ssl_filtered$Participant.Public.ID
  
  # Determine session number and clean participant IDs
  ssl_filtered <- ssl_filtered %>%
    mutate(
      # Create session number column
      session_number = case_when(
        grepl("[_]?[sS]2$", Participant.Public.ID) ~ '2',
        TRUE ~ '1'
      ),
      # Clean ID - first remove session indicators
      Participant.Public.ID = gsub("[_]?[sS]2$", "", Participant.Public.ID),
      # Extract numeric part - UPDATED for "wd _13_" format with space
      Participant.Public.ID = case_when(
        # For IDs like "wd _13_" where there's a space and the number is surrounded by underscores
        grepl("^[Ww][Dd]\\s+_\\d+_$", Participant.Public.ID) ~ as.numeric(gsub("^[Ww][Dd]\\s+_|(\\d+)_$", "\\1", Participant.Public.ID)),
        # For IDs like "wd_14" or "Wd_14"
        grepl("^[Ww][Dd]_\\d+$", Participant.Public.ID) ~ as.numeric(gsub("^[Ww][Dd]_", "", Participant.Public.ID)),
        # For IDs like "Wd13"
        grepl("^[Ww][Dd]\\d+$", Participant.Public.ID) ~ as.numeric(gsub("^[Ww][Dd]", "", Participant.Public.ID)),
        # Generic pattern for digits surrounded by underscores, possibly with spaces
        grepl("_\\d+_", Participant.Public.ID) ~ as.numeric(gsub("^.*_(\\d+)_.*$", "\\1", Participant.Public.ID)),
        # For IDs like "004"
        TRUE ~ as.numeric(Participant.Public.ID)
      )) %>%
    mutate(score = case_when(
      Response == key ~ 1,
      TRUE ~ 0
    )) %>%
    mutate(id = as.character(Participant.Public.ID)) %>%
    dplyr::select(id, task, Trial.Number, session_number, score)
  
  # Create trial-by-trial accuracy dataframe
  cohort2_afc_accuracy_by_trial <<- ssl_filtered %>%
    dplyr::select(id, task, Trial.Number, session_number, score) %>%
    mutate(
      task_label = case_when(
        task == "lsl" ~ "Letter SL",
        task == "ssl" ~ "Syllable SL",
        TRUE ~ as.character(task)
      )
    )
  
  
  accuracy <- ssl_filtered %>%
    group_by(id, session_number, task) %>%
    summarize(
      afc_accuracy = mean(score, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = c(task, session_number), 
      values_from = afc_accuracy,
      names_prefix = "afc_accuracy_"
    ) %>%
    mutate(afc_accuracy_lsl_s1 = afc_accuracy_lsl_1,
           afc_accuracy_lsl_s2 = afc_accuracy_lsl_2,
           afc_accuracy_ssl_s1 = afc_accuracy_ssl_1,
           afc_accuracy_ssl_s2 = afc_accuracy_ssl_2
    ) %>%
    dplyr::select(-c(afc_accuracy_lsl_1, afc_accuracy_lsl_2, afc_accuracy_ssl_1, afc_accuracy_ssl_2
    ))
  
  plot_data <- ssl_filtered %>%
    group_by(id, session_number, task) %>%
    summarize(
      afc_accuracy = mean(score, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Create a factor for task with correct labels
    mutate(
      task_label = case_when(
        task == "lsl" ~ "Letter SL",
        task == "ssl" ~ "Syllable SL",
        TRUE ~ as.character(task)
      ),
      # Make task_label a factor with specific order
      task_label = factor(task_label, levels = c("Letter SL", "Syllable SL")),
      # Make session a factor for plotting
      session = factor(paste("Session", session_number))
    )
  
  # Calculate summary statistics for plotting
  summary_data <- plot_data %>%
    group_by(task_label, session) %>%
    summarize(
      mean_accuracy = mean(afc_accuracy, na.rm = TRUE),
      se = sd(afc_accuracy, na.rm = TRUE) / sqrt(n()),
      n = n(),
      # T-test against chance level (0.5)
      p_value = t.test(afc_accuracy, mu = 0.5)$p.value,
      # Create significance stars
      stars = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01 ~ "**", 
        p_value < 0.05 ~ "*",
        TRUE ~ "ns"
      ),
      .groups = "drop"
    )
  
  p <- ggplot(summary_data, aes(x = session, y = mean_accuracy, fill = session)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.6) +
    geom_errorbar(aes(ymin = mean_accuracy - se, ymax = mean_accuracy + se), width = 0.2) +
    # Add significance stars (only for significant results)
    geom_text(data = . %>% filter(stars != "ns"),
              aes(x = session,  # Use session instead of fixed position
                  y = mean_accuracy + se + 0.08,  # Position above the error bars
                  label = stars),
              size = 8,
              vjust = -0.5) +
    scale_size_identity() +
    facet_wrap(~task_label, scales = "fixed",
               labeller = labeller(task_label = 
                                     c("Letter SL" = "Letter SL", 
                                       "Syllable SL" = "Syllable SL"))) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "darkgray") +
    labs(title = "2AFC Accuracy by Session",
         x = "Session", 
         y = "Mean Accuracy", 
         fill = "Session") +
    scale_fill_manual(values = c("Session 1" = "lightpink1", "Session 2" = "deeppink3")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 34, face = "bold", hjust = 0.5, margin = margin(b = 25)),
      axis.title.x = element_text(size = 22, face = "bold", margin = margin(t = 20)),
      axis.title.y = element_text(size = 22, face = "bold", margin = margin(r = 20)),
      axis.text = element_text(size = 20),
      strip.text = element_text(size = 22, margin = margin(b = 25)),
      legend.position = "none"
    ) +
    ylim(0, 1)  # Set y-axis limit
  
  # Create directory if it doesn't exist
  dir.create("plots", showWarnings = FALSE)
  
  # Save the plot
  plot_file_path <- "~/wd_manuscript/plots/group_level_plots/sl_tasks/cohort2_afc_accuracy.png"
  ggsave(plot_file_path, p, width = 10, height = 8, dpi = 300)
  
  return(accuracy)
}
cohort1_afc_accuracy <- function(csv_files){
  
  # Process each file individually before binding
  processed_data <- lapply(csv_files, function(file) {
    # Read the CSV file
    data <- read.csv(file)
    
    # Convert Spreadsheet..answer_key to numeric if it exists and is a character
    if ("Spreadsheet..answer_key" %in% colnames(data)) {
      if (is.character(data$Spreadsheet..answer_key)) {
        # Convert to numeric first (non-numeric values become NA)
        # Then convert to integer
        data$Spreadsheet..answer_key <- as.integer(as.numeric(data$Spreadsheet..answer_key))
      }
    }
    
    # Convert Participant.Public.ID to character if it's an integer/numeric
    if ("Participant.Public.ID" %in% colnames(data)) {
      if (is.integer(data$Participant.Public.ID) || is.numeric(data$Participant.Public.ID)) {
        data$Participant.Public.ID <- as.character(data$Participant.Public.ID)
      }
    }
    
    # Add task column based on Task.Name
    if ("Task.Name" %in% colnames(data)) {
      data$task <- case_when(
        grepl("LSL", data$Task.Name, ignore.case = TRUE) ~ "lsl",
        grepl("SSL", data$Task.Name, ignore.case = TRUE) ~ "ssl",
        TRUE ~ NA_character_
      )
    }
    
    return(data)
  })
  
  # Bind all data
  all_data <- bind_rows(processed_data) %>%
    dplyr::select(Participant.Public.ID, Spreadsheet..stimuli, Screen, Display, Trial.Number, task, Response) %>%
    filter((Display == 'test_phase' | Display == '2afc_screen')) %>%
    # First filter out blank and silent files
    filter(Spreadsheet..stimuli != 'blank.png' & 
             Spreadsheet..stimuli != 'blank2.png' & 
             Spreadsheet..stimuli != 'silent.wav') %>%
    filter(Response != 'audio started')
  
  # Add session number before deduplication
  all_data <- all_data %>%
    mutate(
      # Create session number column
      session_number = case_when(
        grepl("[_]?[sS]2$", Participant.Public.ID) ~ 2,
        TRUE ~ 1
      ),
      # Store original trial number for deduplication
      original_trial = Trial.Number
    )
  
  # Remove duplicates based on id, stimuli, screen, original_trial, and session_number
  # all_data <- all_data %>%
  #   distinct(Participant.Public.ID, Spreadsheet..stimuli, Screen, original_trial, session_number, Response, .keep_all = TRUE)
  # 
  # Create letter column based on file extension
  all_data <- all_data %>%
    mutate(letter = case_when(
      grepl("\\.wav$", Spreadsheet..stimuli) ~ substring(Spreadsheet..stimuli, 1, 2),
      TRUE ~ substring(Spreadsheet..stimuli, 1, 1)
    ))
  
  # Clean IDs after deduplication but before grouping
  all_data <- all_data %>%
    mutate(
      # Clean ID - remove session indicators and convert to numeric then back to character
      # This will remove leading zeros
      Participant.Public.ID = as.character(as.numeric(gsub("[_]?[sS]2$", "", Participant.Public.ID)))
    )
  
  # Split the data by task to apply different patterns
  lsl_data <- all_data %>% filter(task == "lsl")
  ssl_data <- all_data %>% filter(task == "ssl")
  other_data <- all_data %>% filter(is.na(task))
  
  # Process LSL data - 8 rows per trial (3 option1, 3 option2, 2 NA)
  if(nrow(lsl_data) > 0) {
    lsl_data <- lsl_data %>%
      group_by(Participant.Public.ID) %>%
      mutate(
        # Create a sequence for every 8 rows
        group_num = ceiling(row_number() / 8),
        # Create trial numbers that repeat (1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,...,32,32,32,32,32,32,32,32)
        trial_num = rep(1:32, each = 8, length.out = n())[1:n()],
        # Create option pattern (option1,option1,option1,option2,option2,option2,NA,NA,...)
        option = rep(c(rep("option1", 3), rep("option2", 3), rep(NA, 2)), length.out = n())
      ) %>%
      # Clean up
      ungroup() %>%
      # Replace Trial.Number with trial_num
      mutate(Trial.Number = trial_num) %>%
      # Remove intermediary column
      dplyr::select(-group_num, -trial_num)
  }
  
  # Process SSL data - 6 rows per trial (3 option1, 3 option2)
  if(nrow(ssl_data) > 0) {
    ssl_data <- ssl_data %>%
      group_by(Participant.Public.ID) %>%
      mutate(
        # Create a sequence for every 6 rows
        group_num = ceiling(row_number() / 8),
        # Create trial numbers that repeat (1,1,1,1,1,1,2,2,2,2,2,2,...,32,32,32,32,32,32)
        trial_num = rep(1:32, each = 8, length.out = n())[1:n()],
        # Create option pattern (option1,option1,option1,option2,option2,option2,...)
        option = rep(c(rep("option1", 3), rep("option2", 3), rep(NA, 2)), length.out = n())
      ) %>%
      # Clean up
      ungroup() %>%
      # Replace Trial.Number with trial_num
      mutate(Trial.Number = trial_num) %>%
      # Remove intermediary column
      dplyr::select(-group_num, -trial_num)
  }
  
  # Process other data (if any) with default pattern
  if(nrow(other_data) > 0) {
    other_data <- other_data %>%
      group_by(Participant.Public.ID) %>%
      mutate(
        # Create a sequence for every 8 rows
        group_num = ceiling(row_number() / 8),
        # Create trial numbers that repeat
        trial_num = rep(1:32, each = 8, length.out = n())[1:n()],
        # Create option pattern
        option = rep(c(rep("option1", 3), rep("option2", 3), rep(NA, 2)), length.out = n())
      ) %>%
      # Clean up
      ungroup() %>%
      # Replace Trial.Number with trial_num
      mutate(Trial.Number = trial_num) %>%
      # Remove intermediary column
      dplyr::select(-group_num, -trial_num)
  }
  
  # Combine all data back together
  all_data <- bind_rows(lsl_data, ssl_data, other_data)
  
  all_data <- all_data %>%
    group_by(Participant.Public.ID, Trial.Number, session_number, task) %>%
    mutate(
      # Find the single non-blank Response in each group and apply it to all rows
      Response = ifelse(any(Response != "" & !is.na(Response)),
                        first(Response[Response != "" & !is.na(Response)]),
                        "")
    ) %>%
    ungroup() %>%
    filter(letter != 'i') %>%
    dplyr::select(Participant.Public.ID, Trial.Number, task, Response, session_number, letter, option)
  
  # Add option1 and option2 columns by concatenating letters, then remove option column and keep distinct rows
  all_data <- all_data %>%
    group_by(Participant.Public.ID, Trial.Number, session_number, task) %>%
    mutate(
      # Find the single non-blank Response in each group and apply it to all rows
      Response = ifelse(any(Response != "" & !is.na(Response)),
                        first(Response[Response != "" & !is.na(Response)]),
                        ""),
      # Create option1 column by concatenating letters where option = "option1"
      option1 = paste(letter[option == "option1"], collapse = ""),
      # Create option2 column by concatenating letters where option = "option2"
      option2 = paste(letter[option == "option2"], collapse = "")
    ) %>%
    ungroup() %>%
    dplyr::select(Participant.Public.ID, Trial.Number, task, Response, session_number, option1, option2) %>%
    # Keep only distinct rows based on the key columns
    distinct(Participant.Public.ID, Trial.Number, session_number, task, .keep_all = TRUE) %>%
    mutate(key = case_when(
      option1 == 'pabiku' ~ 1,
      option1 == 'daropi' ~ 1,
      option1 == 'golatu' ~ 1,
      option1 == 'tibudo' ~ 1,
      option2 == 'pabiku' ~ 2,
      option2 == 'daropi' ~ 2,
      option2 == 'golatu' ~ 2,
      option2 == 'tibudo' ~ 2,
      
      option1 == 'GJA' ~ 1,
      option1 == 'FKC' ~ 1,
      option1 == 'LBE' ~ 1,
      option1 == 'MDH' ~ 1,
      option2 == 'GJA' ~ 2,
      option2 == 'FKC' ~ 2,
      option2 == 'LBE' ~ 2,
      option2 == 'MDH' ~ 2,
      TRUE ~ 3
    )) %>%
    filter(key != 3) %>%
    mutate(accuracy = case_when(
      Response == key ~ 1,
      TRUE ~ 0)) %>%
    mutate(id = Participant.Public.ID)
  
  # Calculate the mean accuracy for each participant by session and task
  accuracy_by_participant_task <- all_data %>%
    group_by(id, session_number, task) %>%
    summarize(
      afc_accuracy = mean(accuracy, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Reshape the data to have one row per participant with columns for each task and session
  result <- accuracy_by_participant_task %>%
    pivot_wider(
      id_cols = id,
      names_from = c(task, session_number),
      values_from = afc_accuracy,
      names_glue = "{.value}_{task}_s{session_number}"
    )
  
  # Reshape data for plotting
  plot_data <- accuracy_by_participant_task %>%
    # Create a factor for task with nicer labels
    mutate(
      task_label = case_when(
        task == "lsl" ~ "Letter SL",
        task == "ssl" ~ "Syllable SL",
        TRUE ~ as.character(task)
      ),
      # Make task_label a factor with specific order
      task_label = factor(task_label, levels = c("Letter SL", "Syllable SL")),
      # Make session a factor for plotting
      session = factor(paste("Session", session_number))
    )
  
  # Calculate summary statistics for plotting
  summary_data <- plot_data %>%
    group_by(task_label, session) %>%
    summarize(
      mean_accuracy = mean(afc_accuracy, na.rm = TRUE),
      se = sd(afc_accuracy, na.rm = TRUE) / sqrt(n()),
      n = n(),
      # T-test against chance level (0.5)
      p_value = t.test(afc_accuracy, mu = 0.5)$p.value,
      # Create significance stars
      stars = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01 ~ "**", 
        p_value < 0.05 ~ "*",
        TRUE ~ "ns"
      ),
      # Calculate confidence intervals
      ci_lower = mean_accuracy - qt(0.975, n-1) * se,
      ci_upper = mean_accuracy + qt(0.975, n-1) * se,
      .groups = "drop"
    )
  
  p <- ggplot(summary_data, aes(x = session, y = mean_accuracy, fill = session)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.6) +
    geom_errorbar(aes(ymin = mean_accuracy - se, ymax = mean_accuracy + se), width = 0.2) +
    # Add significance stars (only for significant results)
    geom_text(data = . %>% filter(stars != "ns"),
              aes(x = session,  # Use session instead of fixed position
                  y = mean_accuracy + se + 0.08,  # Position above the error bars
                  label = stars),
              size = 8,
              vjust = -0.5) +
    scale_size_identity() +
    facet_wrap(~task_label, scales = "fixed",
               labeller = labeller(task_label = 
                                     c("Letter SL" = "Letter SL", 
                                       "Syllable SL" = "Syllable SL"))) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "darkgray") +
    labs(title = "2AFC Accuracy by Session",
         x = "Session", 
         y = "Mean Accuracy", 
         fill = "Session") +
    scale_fill_manual(values = c("Session 1" = "lightpink1", "Session 2" = "deeppink3")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 34, face = "bold", hjust = 0.5, margin = margin(b = 25)),
      axis.title.x = element_text(size = 22, face = "bold", margin = margin(t = 20)),
      axis.title.y = element_text(size = 22, face = "bold", margin = margin(r = 20)),
      axis.text = element_text(size = 20),
      strip.text = element_text(size = 22, margin = margin(b = 25)),
      legend.position = "none"
    ) +
    ylim(0, 1)  # Set y-axis limit
  
  # Create directory if it doesn't exist
  dir.create("plots", showWarnings = FALSE)
  
  # Save the plot
  plot_file_path <- "~/wd_manuscript/plots/group_level_plots/sl_tasks/cohort1_afc_accuracy.png"
  ggsave(plot_file_path, p, width = 10, height = 8, dpi = 300)
  
  # Create trial-by-trial accuracy dataframe
  cohort1_afc_accuracy_by_trial <<- all_data %>%
    mutate(
      task_label = case_when(
        task == "lsl" ~ "Letter SL",
        task == "ssl" ~ "Syllable SL",
        TRUE ~ as.character(task)
      )
    ) %>%
    dplyr::select(id, task, task_label, Trial.Number, session_number, accuracy) %>%
    dplyr::rename(score = accuracy)
  
  return(result)
}

# ________ functions for other measures: demographics, assessments ________

# formats demographic info
wd_demographics <- function(df) {
  # Create a copy of the dataframe to avoid modifying the original
  result <- df %>%
    dplyr::mutate(id = as.character(as.numeric(gsub("\\D", "", record_id))))
  
  # Rename the columns
  names(result)[names(result) == "wd_child_dob"] <- "dob"
  names(result)[names(result) == "wd_datetime_1"] <- "time1_date"
  names(result)[names(result) == "wd_gender"] <- "gender"
  
  # Define race mapping
  race_mapping <- c(
    "wd_race___1" = "American Indian / Alaska Native",
    "wd_race___2" = "Asian",
    "wd_race___3" = "Black or African American",
    "wd_race___4" = "Native Hawaiian or Pacific Islander",
    "wd_race___5" = "White",
    "wd_race___6" = "Other / More than one race"
  )
  
  # Create new race column
  if (any(grepl("wd_race___", names(df)))) {
    # Initialize race column
    result$race <- NA
    
    # For each row, process race
    for (i in 1:nrow(result)) {
      # Check if dob is blank
      if (is.na(df$wd_child_dob[i]) || df$wd_child_dob[i] == "") {
        result$race[i] <- NA
      } else {
        # Get race columns for this row
        race_cols <- paste0("wd_race___", 1:6)
        race_cols <- race_cols[race_cols %in% names(df)]
        
        # Check which race values are marked (not NA and not 0)
        marked_races <- character(0)
        for (col in race_cols) {
          if (!is.na(df[[col]][i]) && df[[col]][i] != 0) {
            marked_races <- c(marked_races, col)
          }
        }
        
        # Determine race value
        if (length(marked_races) == 0) {
          result$race[i] <- NA
        } else if (length(marked_races) == 1) {
          result$race[i] <- race_mapping[marked_races]
        } else {
          result$race[i] <- "Other / More than one race"
        }
      }
    }
  }
  
  # Process ethnicity if it exists
  if ("wd_ethnicity" %in% names(df)) {
    names(result)[names(result) == "wd_ethnicity"] <- "ethnicity"
    
    # Map ethnicity values
    for (i in 1:nrow(result)) {
      if (is.na(df$wd_child_dob[i]) || df$wd_child_dob[i] == "") {
        result$ethnicity[i] <- NA
      } else if (is.na(result$ethnicity[i])) {
        result$ethnicity[i] <- "Not Reported"
      } else if (result$ethnicity[i] == 1) {
        result$ethnicity[i] <- "Hispanic or Latino"
      } else if (result$ethnicity[i] == 2) {
        result$ethnicity[i] <- "Not Hispanic or Latino"
      }
    }
  }
  
  # Calculate age - directly parse and handle 2-digit years correctly
  result$age <- NA  # Initialize age column
  
  for (i in 1:nrow(result)) {
    # Skip if either date is missing
    if (is.na(df$wd_datetime_1[i]) || is.na(df$wd_child_dob[i]) ||
        df$wd_datetime_1[i] == "" || df$wd_child_dob[i] == "") {
      next
    }
    
    # Extract date part (remove time component if present)
    time1_str <- sub("\\s+.*$", "", df$wd_datetime_1[i])
    dob_str <- df$wd_child_dob[i]
    
    # Parse dates with specific handling for your formats
    
    # For visit date in format 07/12/23 or similar
    if (grepl("/", time1_str)) {
      parts <- strsplit(time1_str, "/")[[1]]
      if (length(parts) == 3) {
        month <- as.numeric(parts[1])
        day <- as.numeric(parts[2])
        year <- as.numeric(parts[3])
        
        # Handle 2-digit year for visit date
        if (year < 100) {
          if (year > 50) {  # Assume years > 50 are in 1900s
            year <- 1900 + year
          } else {  # Assume years <= 50 are in 2000s
            year <- 2000 + year
          }
        }
        
        visit_date <- as.Date(paste(year, month, day, sep = "-"))
      } else {
        visit_date <- NA
      }
    } else {
      # Try standard parsing
      visit_date <- as.Date(time1_str, format = "%m-%d-%Y")
      if (is.na(visit_date)) {
        visit_date <- as.Date(time1_str, format = "%Y-%m-%d")
      }
    }
    
    # For DOB
    if (grepl("/", dob_str)) {
      parts <- strsplit(dob_str, "/")[[1]]
      if (length(parts) == 3) {
        month <- as.numeric(parts[1])
        day <- as.numeric(parts[2])
        year <- as.numeric(parts[3])
        
        # Handle 2-digit year for DOB
        if (year < 100) {
          if (year > 50) {  # Assume years > 50 are in 1900s
            year <- 1900 + year
          } else {  # Assume years <= 50 are in 2000s
            year <- 2000 + year
          }
        }
        
        birth_date <- as.Date(paste(year, month, day, sep = "-"))
      } else {
        birth_date <- NA
      }
    } else {
      # Try standard parsing
      birth_date <- as.Date(dob_str, format = "%m-%d-%Y")
      if (is.na(birth_date)) {
        birth_date <- as.Date(dob_str, format = "%Y-%m-%d")
      }
    }
    
    # Calculate age only if both dates are valid
    if (!is.na(visit_date) && !is.na(birth_date)) {
      result$age[i] <- as.numeric(difftime(visit_date, birth_date, units = "days")) / 365.25
    }
  }
  
  # Remove original race columns and id column if they exist
  race_cols_to_remove <- grep("^wd_race___", names(result), value = TRUE)
  id_cols_to_remove <- grep("record_id", names(result), value = TRUE)
  if (length(race_cols_to_remove) > 0) {
    result <- result[, !(names(result) %in% race_cols_to_remove)]
  }
  if (length(id_cols_to_remove) > 0) {
    result <- result[, !(names(result) %in% id_cols_to_remove)]
  }
  
  return(result)
}

# formats assessment info; adds difference scores
wd_assessments <- function(df) {
  
  result <- df %>% 
    dplyr::mutate(id = as.character(as.numeric(gsub("\\D", "", id)))) %>% 
    mutate(across(everything(), ~na_if(na_if(., "N/A"), "")))
  
  # Define post-/pre-score pairs for each difference variable
  diff_vars <- list(
    taps_deletion_diff   = c("taps_deletion_ss_post",  "taps_deletion_ss_pre"),
    dibels_nwf_cls_diff  = c("dibels_nwf_cls_post",    "dibels_nwf_cls_pre"),
    dibels_nwf_wrc_diff  = c("dibels_nwf_wrc_post",    "dibels_nwf_wrc_pre"),
    dibels_orf_diff      = c("dibels_orf_post",        "dibels_orf_pre"),
    towre_swe_diff       = c("towre_swe_ss_post",      "towre_swe_ss_pre"),
    towre_pde_diff       = c("towre_pde_ss_post",      "towre_pde_ss_pre")
  )
  
  # Convert the paired columns to numeric and compute the new difference scores
  for (cols in diff_vars) {
    if (all(cols %in% names(df))) {
      result <- result %>% 
        mutate(across(all_of(cols), as.numeric)) %>% 
        mutate(
          !!names(diff_vars)[
            which(vapply(diff_vars, identical, cols, FUN.VALUE = logical(1)))
          ] :=
            ifelse(.data[[cols[2]]] == 1,
                   NA_real_,                                     # avoid division by zero
                   (.data[[cols[1]]] - .data[[cols[2]]]) /       # (s2 - s1)
                     (1 - .data[[cols[2]]])                      # -----------
            )
        )
    }
  }
  
  return(result)
}

# ________ analysis & visualization functions ________

# creates and saves participant table
create_participant_table <- function(data, save_path = NULL, width = 8, height = NULL) {
  # Initialize variables
  n_participants <- if("id" %in% names(data)) n_distinct(data$id) else nrow(data)
  
  # Demographics summary - age
  age_stats <- if("age" %in% names(data)) {
    age_numeric <- as.numeric(as.character(data$age))
    list(
      age_mean = round(mean(age_numeric, na.rm = TRUE), 2),
      age_sd = round(sd(age_numeric, na.rm = TRUE), 2),
      age_range = paste(round(min(age_numeric, na.rm = TRUE), 2), "-", 
                        round(max(age_numeric, na.rm = TRUE), 2)),
      age_na = sum(is.na(data$age))
    )
  } else {
    list(age_mean = "Not Reported", age_sd = "Not Reported", 
         age_range = "Not Reported", age_na = nrow(data))
  }
  
  # Helper function for distribution summaries
  get_distribution <- function(col_name, alternatives = NULL, transform_fn = NULL) {
    col_to_use <- if(!is.null(alternatives) && any(alternatives %in% names(data))) {
      alternatives[alternatives %in% names(data)][1]
    } else col_name
    
    if(col_to_use %in% names(data)) {
      data %>%
        mutate(value = if(!is.null(transform_fn)) transform_fn(.data[[col_to_use]]) 
               else ifelse(is.na(.data[[col_to_use]]), "Not Reported", .data[[col_to_use]])) %>%
        group_by(value) %>%
        summarize(
          count = n(),
          percentage = round(n() / nrow(data) * 100, 1)
        ) %>%
        mutate(display = paste0(count, " (", percentage, "%)")) %>%
        arrange(desc(value == "Not Reported"))
    } else {
      data.frame(
        value = "Not Reported",
        count = nrow(data),
        percentage = 100,
        display = paste0(nrow(data), " (100%)")
      )
    }
  }
  
  # Get distributions
  gender_dist <- get_distribution("gender", alternatives = c("participant_gender", "gender"))
  names(gender_dist)[1] <- "gender"
  
  race_dist <- get_distribution("race")
  names(race_dist)[1] <- "race"
  
  ethnicity_transform <- function(x) {
    ifelse(is.na(x) | x == "Not Reported", "Not Reported", 
           ifelse(x == "Yes", "Hispanic or Latino", "Not Hispanic or Latino"))
  }
  ethnicity_dist <- get_distribution("ethnicity", transform_fn = ethnicity_transform)
  names(ethnicity_dist)[1] <- "ethnicity"
  
  # Create table rows
  table_rows <- list(c("Participants, n", n_participants))
  
  # Add age rows
  if("age" %in% names(data)) {
    age_mean_sd <- if(is.character(age_stats$age_mean)) "Not Reported" 
    else paste0(age_stats$age_mean, " (", age_stats$age_sd, ")")
    table_rows <- c(table_rows, 
                    list(c("Age (years)", "")),
                    list(c("  Mean (SD)", age_mean_sd)),
                    list(c("  Range", age_stats$age_range)))
  }
  
  # Add demographic rows
  add_category_rows <- function(category_name, dist_df, col_name) {
    rows <- list(c(paste0(category_name, ", n (%)"), ""))
    for(i in 1:nrow(dist_df)) {
      rows <- c(rows, list(c(paste0("  ", dist_df[[col_name]][i]), dist_df$display[i])))
    }
    return(rows)
  }
  
  table_rows <- c(table_rows, add_category_rows("Gender", gender_dist, "gender"))
  if("race" %in% names(data)) {
    table_rows <- c(table_rows, add_category_rows("Race", race_dist, "race"))
  }
  if("ethnicity" %in% names(data)) {
    table_rows <- c(table_rows, add_category_rows("Ethnicity", ethnicity_dist, "ethnicity"))
  }
  
  # Define assessment columns with their labels
  assessment_map <- list(
    "taps_deletion_ss" = "TAPS Deletion",
    "taps_blending_ss" = "TAPS Blending",
    "dibels_nwf_cls" = "DIBELS Nonword Fluency Correct Letter Sounds",
    "dibels_nwf_wrc" = "DIBELS Nonword Fluency Words Correct",
    "dibels_orf" = "DIBELS Oral Reading Fluency",
    "towre_swe_ss" = "TOWRE Sight Word Efficiency",
    "towre_pde_ss" = "TOWRE Phonemic Decoding Efficiency"
  )
  
  # Add assessment rows if available
  has_assessments <- FALSE
  for(assessment_col in names(assessment_map)) {
    pre_col <- paste0(assessment_col, "_pre")
    post_col <- paste0(assessment_col, "_post")
    
    if(pre_col %in% names(data) || post_col %in% names(data)) {
      if(!has_assessments) {
        table_rows <- c(table_rows, list(c("Assessment Scores, Mean (SD)", "")))
        has_assessments <- TRUE
      }
      
      get_stat <- function(col) {
        if(col %in% names(data)) {
          values <- as.numeric(as.character(data[[col]]))
          sprintf("%.2f (%.2f)", round(mean(values, na.rm = TRUE), 2), round(sd(values, na.rm = TRUE), 2))
        } else "Not Available"
      }
      
      table_rows <- c(table_rows, list(c(
        paste0(assessment_map[[assessment_col]], " (Pre | Post)"),
        paste0(get_stat(pre_col), " | ", get_stat(post_col))
      )))
    }
  }
  
  # Convert to data frame
  table_df <- do.call(rbind, table_rows) %>% as.data.frame()
  names(table_df) <- c("Characteristic", "Value")
  
  # Create the table using gridExtra - IMPROVED FORMATTING
  table_plot <- gridExtra::tableGrob(
    table_df, 
    rows = NULL, 
    theme = gridExtra::ttheme_default(
      base_size = 10,
      core = list(
        fg_params = list(hjust = 0, x = 0.05, fontsize = 10),
        bg_params = list(fill = rep(c("white", "grey95"), length.out = nrow(table_df)))
      ),
      colhead = list(
        fg_params = list(fontface = "bold", hjust = c(0, 0.5), x = c(0.05, 0.5)),
        bg_params = list(fill = "grey90")
      )
    )
  )
  
  table_plot$widths <- grid::unit(c(0.7, 0.3), "npc")
  
  # Create a plot with the table
  p <- ggplot() + 
    annotation_custom(table_plot) +
    labs(title = "Participant Characteristics") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
  
  # Save path is designated here
  save_path <- paste0("~/wd_manuscript/plots/demographic_tables/", 
                      substr(deparse(substitute(data)), 
                             nchar(deparse(substitute(data)))-7, 
                             nchar(deparse(substitute(data)))), 
                      "_participant_table.png")
  
  # Save the plot if a path is provided
  if (!is.null(save_path)) {
    height <- if(is.null(height)) 0.25 * nrow(table_df) + 1 else height
    ggsave(filename = save_path, plot = p, width = width, height = height, units = "in")
  }
  
  # Return both the raw data frame and plot
  return(list(table_data = table_df, plot = p))
}

# completes lmer test for ssl task; produces bar plot visualizations
wd_lmer_test <- function(df, score_suffix = NULL, df_name = NULL) {
  
  # Set the multiplier for adjusting the space above the highest bar
  star_distance_multiplier <- 0.25
  
  # Get the dataframe name if not provided
  if(is.null(df_name)) {
    df_name_expr <- substitute(df)
    if(is.name(df_name_expr)) {
      df_name <- as.character(df_name_expr)
    } else {
      df_name <- "lmer_results"
    }
  }
  
  # Check and auto-detect task type from dataframe name if not found in columns
  task_type <- NULL
  if(grepl("lsl", df_name, ignore.case = TRUE)) {
    task_type <- "lsl"
  } else if(grepl("ssl", df_name, ignore.case = TRUE)) {
    task_type <- "ssl"
  }
  
  # Determine score column names based on the dataframe
  if(is.null(score_suffix)) {
    # Auto-detect column patterns based on task type
    if(task_type == "lsl") {
      # For LSL, check for letters columns
      possible_prefixes <- c("correct_serial_letters", "correct_serial_syllables")
    } else {
      # For SSL or unknown, check for syllables columns
      possible_prefixes <- c("correct_serial_syllables", "correct_serial_letters")
    }
    
    # Try to find matching columns with various suffixes
    found_columns <- FALSE
    for(prefix in possible_prefixes) {
      possible_suffixes <- c(".lsl", ".ssl", "")
      for(suffix in possible_suffixes) {
        syllable_col <- paste0(prefix, suffix)
        bigram_col <- paste0("correct_bigrams", suffix)
        
        if(syllable_col %in% names(df) && bigram_col %in% names(df)) {
          found_columns <- TRUE
          score_suffix <- suffix
          serial_prefix <- prefix
          break
        }
      }
      if(found_columns) break
    }
    
    if(!found_columns) {
      stop("Could not automatically determine score column names. Please specify score_suffix parameter.")
    }
    
    syllable_col <- paste0(serial_prefix, score_suffix)
    bigram_col <- paste0("correct_bigrams", score_suffix)
  } else {
    # Use provided suffix, but try both syllables and letters
    if(task_type == "lsl") {
      # For LSL, check letters first
      if(paste0("correct_serial_letters", score_suffix) %in% names(df)) {
        syllable_col <- paste0("correct_serial_letters", score_suffix)
      } else {
        syllable_col <- paste0("correct_serial_syllables", score_suffix)
      }
    } else {
      # For SSL or unknown, check syllables first
      if(paste0("correct_serial_syllables", score_suffix) %in% names(df)) {
        syllable_col <- paste0("correct_serial_syllables", score_suffix)
      } else {
        syllable_col <- paste0("correct_serial_letters", score_suffix)
      }
    }
    
    bigram_col <- paste0("correct_bigrams", score_suffix)
    
    # Check if columns exist
    if(!(syllable_col %in% names(df) && bigram_col %in% names(df))) {
      stop("Specified score columns not found in the dataframe.")
    }
  }
  
  # Determine the display name for the serial measure based on the column name
  if(grepl("letters", syllable_col)) {
    serial_display_name <- "Correct Serial Letters"
  } else {
    serial_display_name <- "Correct Serial Syllables"
  }
  
  # Prepare data for analysis - convert to long format
  long_df <- df %>%
    dplyr::select(id, target_tag, !!sym(syllable_col), !!sym(bigram_col)) %>%
    pivot_longer(
      cols = c(!!sym(syllable_col), !!sym(bigram_col)),
      names_to = "score_type",
      values_to = "score"
    ) %>%
    mutate(
      score_type = case_when(
        grepl("serial_letters", score_type) ~ "serial",
        grepl("serial_syllables", score_type) ~ "serial",
        grepl("bigrams", score_type) ~ "bigram",
        TRUE ~ score_type
      )
    )
  
  # Create labels for plotting
  condition_labels <- c("0" = "Foil", "1" = "Target")
  long_df <- long_df %>%
    mutate(condition = factor(target_tag),
           condition_label = factor(condition_labels[as.character(target_tag)]))
  
  # Run LMER tests
  print("Running linear mixed effects models...")
  
  # For serial measure (syllables or letters)
  serial_model <- lmer(score ~ condition + (1|id), 
                       data = filter(long_df, score_type == "serial"))
  serial_summary <- summary(serial_model)
  
  # For bigrams
  bigram_model <- lmer(score ~ condition + (1|id), 
                       data = filter(long_df, score_type == "bigram"))
  bigram_summary <- summary(bigram_model)
  
  # Calculate means and standard errors for plotting
  df_summary <- long_df %>%
    group_by(score_type, condition, condition_label) %>%
    summarize(
      mean_score = mean(score, na.rm = TRUE),
      se = sd(score, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
  
  # Get p-values for significance stars
  serial_p_value <- round(serial_summary$coefficients[2, "Pr(>|t|)"], 4)
  bigram_p_value <- round(bigram_summary$coefficients[2, "Pr(>|t|)"], 4)
  
  # Create significance annotation dataframes - one for each score type
  serial_signif <- data.frame(
    score_type = "serial",
    label = case_when(
      serial_p_value < 0.001 ~ "***",
      serial_p_value < 0.01 ~ "**",
      serial_p_value < 0.05 ~ "*",
      serial_p_value < 0.10 ~ ".",
      TRUE ~ "ns"
    )
  )
  
  bigram_signif <- data.frame(
    score_type = "bigram",
    label = case_when(
      bigram_p_value < 0.001 ~ "***",
      bigram_p_value < 0.01 ~ "**",
      bigram_p_value < 0.05 ~ "*",
      bigram_p_value < 0.10 ~ ".",
      TRUE ~ "ns"
    )
  )
  
  # Combine significance dataframes
  signif_df <- rbind(serial_signif, bigram_signif)
  
  # Skip "ns" labels
  signif_df <- signif_df[signif_df$label != "ns", ]
  
  # Calculate y positions for significance stars
  if(nrow(signif_df) > 0) {
    signif_df$y <- sapply(signif_df$score_type, function(st) {
      max_val <- max(df_summary$mean_score[df_summary$score_type == st] + 
                       df_summary$se[df_summary$score_type == st])
      return(max_val * (1 + star_distance_multiplier))
    })
    
    # Set x position for significance stars
    signif_df$x <- 1.5
  }
  
  # Determine score_type suffix for plot title
  plot_title <- if(is.null(score_suffix) || score_suffix == "") {
    "Target vs Foil Discrimination"
  } else {
    paste0("Target vs Foil Discrimination: ", toupper(substring(score_suffix, 2)))
  }
  
  # Create plot with significance stars
  plot <- ggplot(df_summary, aes(x = condition_label, y = mean_score, fill = condition_label)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.6) +
    geom_errorbar(aes(ymin = mean_score - se, ymax = mean_score + se), width = 0.2) +
    facet_wrap(~score_type, scales = "fixed",
               labeller = as_labeller(c("bigram" = "Correct Bigrams", 
                                        "serial" = serial_display_name))) +
    labs(title = plot_title,
         x = "Condition", y = "Mean Score", fill = "Condition") +
    scale_fill_manual(values = c("lightpink1", "deeppink3"), 
                      labels = condition_labels) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 34, face = "bold", hjust = 0.5, margin = margin(b = 25)),
      axis.title.x = element_text(size = 22, face = "bold", margin = margin(t = 20)),
      axis.title.y = element_text(size = 22, face = "bold", margin = margin(r = 20)),
      axis.text = element_text(size = 20),
      strip.text = element_text(size = 22, margin = margin(b = 25)),
      legend.position = "none"
    )
  
  # Add significance stars only if there are significant results
  if(nrow(signif_df) > 0) {
    plot <- plot + 
      geom_text(data = signif_df, 
                aes(x = x, y = y, label = label),
                inherit.aes = FALSE,
                size = 8,
                vjust = -0.5)
  }
  
  # Generate text summary of results
  serial_effect <- round(serial_summary$coefficients[2, "Estimate"], 4)
  bigram_effect <- round(bigram_summary$coefficients[2, "Estimate"], 4)
  
  serial_sig <- ifelse(serial_p_value < 0.05, "significant", "not significant")
  bigram_sig <- ifelse(bigram_p_value < 0.05, "significant", "not significant")
  
  # Adjust the result summary based on which columns we found
  serial_measure_name <- ifelse(grepl("letters", syllable_col), "Serial Letters", "Serial Syllables")
  
  result_summary <- paste0(
    "LMER Analysis Results", if(!is.null(score_suffix) && score_suffix != "") paste0(" for ", substring(score_suffix, 2)), ":\n\n",
    serial_measure_name, ": Target vs Foil difference is ", serial_sig, 
    " (Effect size = ", serial_effect, ", p = ", serial_p_value, ")\n",
    "Bigrams: Target vs Foil difference is ", bigram_sig,
    " (Effect size = ", bigram_effect, ", p = ", bigram_p_value, ")\n"
  )
  
  # Print the result summary to console
  print(result_summary)
  
  # Extract cohort number (if present)
  cohort_num <- str_extract(df_name, "cohort[0-9]+")
  if(is.na(cohort_num)) {
    cohort_num <- ""
  }
  
  # Extract session number (if present)
  session_num <- str_extract(df_name, "s[0-9]+")
  if(is.na(session_num)) {
    # Try to extract from the dataframe if present
    if("session_number" %in% names(df)) {
      # Use the first session number in the dataframe
      session_num <- paste0("s", unique(df$session_number)[1])
    } else {
      session_num <- ""
    }
  }
  
  # Extract task type (lsl or ssl) from either dataframe name or the detected columns
  if(is.null(task_type)) {
    if(grepl("letters", syllable_col)) {
      task_type <- "lsl"
    } else if(grepl("syllables", syllable_col)) {
      task_type <- "ssl" 
    } else if(!is.null(score_suffix) && score_suffix != "") {
      task_type <- substring(score_suffix, 2)  # Remove the dot
    }
  }
  
  # Create the filename according to the specified convention
  # Format: cohort1_group_level_ssl_s1.png
  if(cohort_num != "") {
    file_name <- paste0(cohort_num, "_group_level")
    if(!is.null(task_type)) {
      file_name <- paste0(file_name, "_", task_type)
    }
    if(session_num != "") {
      file_name <- paste0(file_name, "_", session_num)
    }
    file_name <- paste0(file_name, ".png")
  } else {
    # Use a generic name if cohort number not found
    file_name <- paste0("sicr")
    if(!is.null(task_type)) {
      file_name <- paste0(file_name, "_", task_type)
    }
    if(session_num != "") {
      file_name <- paste0(file_name, "_", session_num)
    }
    file_name <- paste0(file_name, ".png")
  }
  
  file_path <- file.path("~/wd_manuscript/plots/group_level_plots/sl_tasks", file_name)
  
  # Save the plot
  ggsave(
    filename = file_path,
    plot = plot,
    width = 10,
    height = 7,
    dpi = 300
  )
  
  cat("Plot saved as:", file_name, "\n")
  
  # Return results
  return(list(
    plot = plot,
    summary_text = result_summary,
    serial_model = serial_model,
    bigram_model = bigram_model,
    data = long_df,
    file_name = file_name
  ))
}

# completes lmer test for difference in performance between s1 and s2
analyze_afc_performance <- function(df) {
  # Load required packages
  if (!require("lme4")) install.packages("lme4")
  if (!require("lmerTest")) install.packages("lmerTest")
  library(lme4)
  library(lmerTest)
  library(dplyr)
  
  # Reshape data from wide to long format for analysis
  long_df <- data.frame(
    id = rep(df$id, 2),
    session = c(rep("s1", nrow(df)), rep("s2", nrow(df))),
    accuracy = c(df$afc_accuracy_ssl_s1, df$afc_accuracy_ssl_s2)
  )
  
  # Convert session to factor
  long_df$session <- factor(long_df$session)
  long_df$id <- factor(long_df$id)
  
  # Run the linear mixed effects model
  # Fixed effects: session
  # Random effects: random intercept for each participant (id)
  model <- lmer(accuracy ~ session + (1|id), data = long_df)
  
  # Get summary of the model
  model_summary <- summary(model)
  
  # Get ANOVA results for fixed effects
  anova_results <- anova(model)
  
  # Create a detailed report
  cat("Linear Mixed Effects Model Analysis: S1 vs S2 Performance\n")
  cat("=====================================================\n\n")
  
  cat("Model Formula:\n")
  cat("  accuracy ~ session + (1|id)\n\n")
  
  cat("Fixed Effects:\n")
  print(model_summary$coefficients)
  cat("\n")
  
  cat("ANOVA Table for Fixed Effects:\n")
  print(anova_results)
  cat("\n")
  
  cat("Random Effects:\n")
  print(VarCorr(model))
  cat("\n")
  
  # Calculate means by session for easier interpretation
  cat("Group Means:\n")
  group_means <- long_df %>%
    group_by(session) %>%
    summarize(mean_accuracy = mean(accuracy, na.rm = TRUE),
              sd_accuracy = sd(accuracy, na.rm = TRUE),
              n = n())
  print(group_means)
  
  # Return the model for further analysis if needed
  return(list(model = model, 
              summary = model_summary, 
              anova = anova_results,
              group_means = group_means))
}

# completes t-test for improvements in assessment scores; produces bar plot visualizations
compare_pre_post <- function(df, include_marginal = TRUE) {
  # Required packages
  require(ggplot2)
  require(dplyr)
  require(stringr)
  
  # Extract dataframe name for file naming
  df_name_expr <- substitute(df)
  if(is.name(df_name_expr)) {
    df_name <- as.character(df_name_expr)
  } else {
    df_name <- "pre_post_comparison"
  }
  
  # Extract cohort number if present
  cohort_match <- str_extract(df_name, "cohort_?[0-9]+")
  if(!is.na(cohort_match)) {
    # Extract just the number part
    cohort_num <- str_extract(cohort_match, "[0-9]+")
    # Format as "cohort#" for filenames
    cohort_name <- paste0("cohort", cohort_num)
  } else {
    cohort_num <- ""
    cohort_name <- ""
  }
  
  # Set color scheme based on cohort
  if(cohort_num == "1") {
    # Purple color scheme for cohort 1
    pre_color <- "#d5bae6"
    post_color <- "#63278b"
  } else if(cohort_num == "2") {
    # Pink color scheme for cohort 2
    pre_color <- "#ffc9db"
    post_color <- "#981340"
  } else {
    # Default color scheme
    pre_color <- "#d5bae6"
    post_color <- "#63278b"
  }
  
  # Find all pre/post pairs
  pre_cols <- colnames(df)[grep("_pre$", colnames(df))]
  significant_results <- list()
  marginal_results <- list()
  
  for (pre_col in pre_cols) {
    # Find corresponding post column
    post_col <- sub("_pre$", "_post", pre_col)
    
    # Check if post column exists
    if (post_col %in% colnames(df)) {
      # Convert non-numeric columns to numeric, replacing non-numeric values with NA
      if(!is.numeric(df[[pre_col]])) {
        cat("Converting", pre_col, "to numeric (non-numeric values will become NA)\n")
        df[[pre_col]] <- as.numeric(as.character(df[[pre_col]]))
      }
      
      if(!is.numeric(df[[post_col]])) {
        cat("Converting", post_col, "to numeric (non-numeric values will become NA)\n")
        df[[post_col]] <- as.numeric(as.character(df[[post_col]]))
      }
      
      # Check for enough non-NA values for paired t-test
      valid_pairs <- sum(!is.na(df[[pre_col]]) & !is.na(df[[post_col]]))
      if(valid_pairs < 2) {
        cat("Warning: Skipping", assessment_name, "as there are not enough paired observations.\n")
        next
      }
      
      # Run one-tailed paired t-test
      tryCatch({
        result <- t.test(df[[post_col]], df[[pre_col]], 
                         alternative = "greater", paired = TRUE)
      }, error = function(e) {
        cat("Error in t-test for", assessment_name, ":", e$message, "\n")
        return(NULL)
      })
      
      # Skip if t-test failed
      if(is.null(result)) next
      
      # Extract assessment name for plot title and file name
      assessment_name <- gsub("_pre$", "", pre_col)
      
      # Make assessment name more readable for plot title
      plot_title <- gsub("_", " ", assessment_name)
      plot_title <- paste0(toupper(substr(plot_title, 1, 1)), 
                           substr(plot_title, 2, nchar(plot_title)),
                           " Improvement")
      
      # Save significant results (p < 0.05) or marginal results (p < 0.1)
      if (result$p.value < 0.05 || (include_marginal && result$p.value < 0.1)) {
        # Check if columns are numeric
        if(!is.numeric(df[[pre_col]]) || !is.numeric(df[[post_col]])) {
          cat("Warning: Skipping", assessment_name, "as one or both columns are not numeric.\n")
          next
        }
        
        # Calculate means and standard errors
        pre_mean <- mean(df[[pre_col]], na.rm = TRUE)
        post_mean <- mean(df[[post_col]], na.rm = TRUE)
        pre_se <- sd(df[[pre_col]], na.rm = TRUE) / sqrt(sum(!is.na(df[[pre_col]])))
        post_se <- sd(df[[post_col]], na.rm = TRUE) / sqrt(sum(!is.na(df[[post_col]])))
        
        # Create data frame for plotting
        plot_data <- data.frame(
          time = factor(c("Pre-intervention", "Post-intervention"), levels = c("Pre-intervention", "Post-intervention")),
          mean_score = c(pre_mean, post_mean),
          se = c(pre_se, post_se)
        )
        
        # Create significance annotation
        p_value <- result$p.value
        sig_label <- ifelse(p_value < 0.001, "***",
                            ifelse(p_value < 0.01, "**",
                                   ifelse(p_value < 0.05, "*", 
                                          ifelse(p_value < 0.1, "†", "ns"))))
        
        # Maximum y value for star placement with multiplier
        star_distance_multiplier <- 0.25
        max_y <- max(plot_data$mean_score + plot_data$se) * (1 + star_distance_multiplier)
        
        # Create plot
        plot <- ggplot(plot_data, aes(x = time, y = mean_score, fill = time)) +
          geom_bar(stat = "identity", position = position_dodge(), width = 0.6) +
          geom_errorbar(aes(ymin = mean_score - se, ymax = mean_score + se), width = 0.2) +
          labs(title = plot_title,
               x = "Assessment Time", y = "Score") +
          scale_fill_manual(values = c("Pre-intervention" = pre_color, "Post-intervention" = post_color)) +
          geom_text(aes(x = 1.5, y = max_y, label = sig_label), size = 8) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 34, face = "bold", hjust = 0.5, margin = margin(b = 25)),
            axis.title.x = element_text(size = 22, face = "bold", margin = margin(t = 20)),
            axis.title.y = element_text(size = 22, face = "bold", margin = margin(r = 20)),
            axis.text = element_text(size = 20),
            legend.position = "none"
          )
        
        # Create file name with indicator for marginal significance
        sig_indicator <- ifelse(p_value < 0.05, "", "_marginal")
        if(cohort_name != "") {
          file_name <- paste0(cohort_name, "_", assessment_name, "_improvement", sig_indicator, ".png")
        } else {
          file_name <- paste0(assessment_name, "_improvement", sig_indicator, ".png")
        }
        file_path <- file.path("~/wd_manuscript/plots/group_level_plots/assessment_improvements", file_name)
        
        # Save plot
        ggsave(
          filename = file_path,
          plot = plot,
          width = 10,
          height = 7,
          dpi = 300
        )
        
        cat("Plot saved as:", file_name, "\n")
        
        # Add to appropriate results list
        result_details <- list(
          plot = plot,
          p_value = p_value,
          t_value = result$statistic,
          df = result$parameter,
          file_name = file_name,
          pre_mean = pre_mean,
          post_mean = post_mean,
          improvement = post_mean - pre_mean,
          percent_improvement = ((post_mean - pre_mean) / pre_mean) * 100
        )
        
        if (p_value < 0.05) {
          significant_results[[assessment_name]] <- result_details
        } else if (include_marginal && p_value < 0.1) {
          marginal_results[[assessment_name]] <- result_details
        }
      }
    }
  }
  
  # Create a combined list of results
  all_results <- list(
    significant = significant_results,
    marginal = marginal_results
  )
  
  # Report on results
  sig_count <- length(significant_results)
  marg_count <- length(marginal_results)
  
  if (sig_count == 0 && marg_count == 0) {
    cat("No significant or marginal improvements found.\n")
  } else {
    if (sig_count > 0) {
      cat(sig_count, "significant improvement(s) found (p < 0.05).\n")
    }
    if (include_marginal && marg_count > 0) {
      cat(marg_count, "marginal improvement(s) found (0.05 ≤ p < 0.1).\n")
    }
  }
  
  # Return list of all results
  return(invisible(all_results))
}

# compute reliability stats: test-retest and internal
test_retest <- function(df, task = NULL, save_to_file = TRUE, filename_prefix = "~/wd_manuscript/plots/reliability/test_retest") {
  # Filter columns based on task (ssl or lsl)
  task_cols <- grep(paste0("\\.", task), names(df), value = TRUE)
  
  # Get unique measurement types (without _s1, _s2, _0, _1 suffixes)
  raw_measurement_types <- unique(gsub("\\.[^.]*_[01]_s[12]$", "", task_cols))
  
  # Filter out total_syllables and total_letters
  measurement_types <- raw_measurement_types[!grepl("total_syllables|total_letters", raw_measurement_types)]
  
  # Check for afc_accuracy columns with correct naming pattern
  # The actual column names are afc_accuracy_ssl_s1 and afc_accuracy_ssl_s2 (or lsl)
  afc_accuracy_s1 <- paste0("afc_accuracy_", task, "_s1")
  afc_accuracy_s2 <- paste0("afc_accuracy_", task, "_s2")
  
  if (all(c(afc_accuracy_s1, afc_accuracy_s2) %in% names(df))) {
    measurement_types <- c(measurement_types, "afc_accuracy")
  }
  
  # Define the desired order - now include afc_accuracy
  ordered_types <- c(
    grep("afc_accuracy", measurement_types, value = TRUE),
    grep("serial_syllables", measurement_types, value = TRUE),
    grep("serial_letters", measurement_types, value = TRUE),
    grep("bigrams", measurement_types, value = TRUE),
    grep("trigrams", measurement_types, value = TRUE)
  )
  
  # Initialize results matrices
  r_values <- matrix(NA, nrow = length(ordered_types), ncol = length(ordered_types))
  p_values <- matrix(NA, nrow = length(ordered_types), ncol = length(ordered_types))
  rownames(r_values) <- colnames(r_values) <- ordered_types
  rownames(p_values) <- colnames(p_values) <- ordered_types
  
  # Compute partial correlations for each measurement type
  for (measure in ordered_types) {
    # Special case for afc_accuracy which has a different column naming pattern
    if (measure == "afc_accuracy") {
      s1_col <- paste0(measure, "_", task, "_s1")
      s2_col <- paste0(measure, "_", task, "_s2")
      
      # Check if both columns exist
      if (all(c(s1_col, s2_col) %in% names(df))) {
        # Create data frame with only the required columns
        temp_df <- df[, c(s1_col, s2_col)]
        temp_df <- temp_df[complete.cases(temp_df), ]
        
        # Skip if not enough data
        if (nrow(temp_df) < 5) {
          next
        }
        
        # Compute regular correlation (not partial) for afc_accuracy
        cor_result <- cor.test(temp_df[[s1_col]], temp_df[[s2_col]])
        
        # Store results
        measure_index <- which(ordered_types == measure)
        r_values[measure_index, measure_index] <- cor_result$estimate
        p_values[measure_index, measure_index] <- cor_result$p.value
      }
    } else {
      # Extract relevant columns for other metrics (using partial correlation)
      s1_target_col <- paste0(measure, ".", task, "_1_s1")
      s1_foil_col <- paste0(measure, ".", task, "_0_s1")
      s2_target_col <- paste0(measure, ".", task, "_1_s2")
      
      # Check if all columns exist
      if (all(c(s1_target_col, s1_foil_col, s2_target_col) %in% names(df))) {
        # Create data frame with only the required columns
        temp_df <- df[, c(s1_target_col, s1_foil_col, s2_target_col)]
        temp_df <- temp_df[complete.cases(temp_df), ]
        
        # Skip if not enough data
        if (nrow(temp_df) < 5) {
          next
        }
        
        # Compute partial correlation
        pcor_result <- pcor.test(temp_df[[s1_target_col]], 
                                 temp_df[[s2_target_col]], 
                                 temp_df[[s1_foil_col]])
        
        # Store results
        measure_index <- which(ordered_types == measure)
        r_values[measure_index, measure_index] <- pcor_result$estimate
        p_values[measure_index, measure_index] <- pcor_result$p.value
      }
    }
  }
  
  # Create heatmaps
  r_df <- melt(r_values)
  names(r_df) <- c("Measure1", "Measure2", "r")
  
  p_df <- melt(p_values)
  names(p_df) <- c("Measure1", "Measure2", "p")
  
  # Combine r and p values
  plot_df <- merge(r_df, p_df, by = c("Measure1", "Measure2"))
  
  # Create labels based on significance
  plot_df$label <- case_when(
    plot_df$p < 0.001 ~ sprintf("R = %.2f***", plot_df$r),
    plot_df$p < 0.01 ~ sprintf("R = %.2f**", plot_df$r),
    plot_df$p < 0.05 ~ sprintf("R = %.2f*", plot_df$r),
    TRUE ~ sprintf("%.2f", plot_df$r)
  )
  
  # Filter out NA values
  plot_df <- plot_df[!is.na(plot_df$r), ]
  
  # Create clean measure names for display
  clean_measure_names <- function(name) {
    # Vectorized version that handles both single strings and vectors
    result <- character(length(name))
    
    for (i in seq_along(name)) {
      if (name[i] == "afc_accuracy") result[i] <- "2AFC Accuracy"
      else if (grepl("serial_syllables", name[i])) result[i] <- "SICR: Correct Serial Syllables"
      else if (grepl("bigrams", name[i])) result[i] <- "SICR: Correct Bigrams"
      else if (grepl("trigrams", name[i])) result[i] <- "SICR: Correct Trigrams"
      else if (grepl("correct_serial_letters", name[i])) result[i] <- "SICR: Correct Serial Letters"
      else result[i] <- name[i]  # fallback
    }
    
    return(result)
  }
  
  # Create factor levels to ensure consistent ordering
  ordered_clean_names <- sapply(ordered_types, clean_measure_names)
  
  plot_df$CleanMeasure1 <- clean_measure_names(plot_df$Measure1)
  plot_df$CleanMeasure2 <- clean_measure_names(plot_df$Measure2)
  
  plot_df$CleanMeasure1 <- factor(plot_df$CleanMeasure1, levels = ordered_clean_names)
  plot_df$CleanMeasure2 <- factor(plot_df$CleanMeasure2, levels = ordered_clean_names)
  
  # Force text wrapping by replacing the measure names with wrapped versions
  wrapped_measure_names <- c(
    "2AFC\nAccuracy",
    "SICR: Correct Serial\nSyllables", 
    "SICR: Correct\nBigrams", 
    "SICR: Correct\nTrigrams"
  )
  
  ordered_wrapped_names <- wrapped_measure_names[
    match(ordered_clean_names, c("2AFC Accuracy", "SICR: Correct Serial Syllables", "SICR: Correct Bigrams", "SICR: Correct Trigrams"))
  ]
  
  plot_df$CleanMeasure1 <- factor(
    case_when(
      plot_df$Measure1 == "afc_accuracy" ~ "2AFC\nAccuracy",
      grepl("serial_syllables", plot_df$Measure1) ~ "SICR: Correct Serial\nSyllables",
      grepl("bigrams", plot_df$Measure1) ~ "SICR: Correct\nBigrams",
      grepl("trigrams", plot_df$Measure1) ~ "SICR: Correct\nTrigrams",
      TRUE ~ as.character(plot_df$CleanMeasure1)
    ),
    levels = wrapped_measure_names[!is.na(wrapped_measure_names)]
  )
  
  plot_df$CleanMeasure2 <- factor(
    case_when(
      plot_df$Measure2 == "afc_accuracy" ~ "2AFC\nAccuracy",
      grepl("serial_syllables", plot_df$Measure2) ~ "SICR: Correct Serial\nSyllables",
      grepl("bigrams", plot_df$Measure2) ~ "SICR: Correct\nBigrams",
      grepl("trigrams", plot_df$Measure2) ~ "SICR: Correct\nTrigrams",
      TRUE ~ as.character(plot_df$CleanMeasure2)
    ),
    levels = wrapped_measure_names[!is.na(wrapped_measure_names)]
  )
  
  # Create the heatmap
  p <- ggplot(plot_df, aes(x = CleanMeasure1, y = CleanMeasure2, fill = r)) +
    geom_tile() +
    geom_text(aes(label = label), size = 5) +
    scale_fill_gradient2(low = "#5b0c6c", mid = "white", high = "#981340", 
                         midpoint = 0, limits = c(-1, 1)) +
    theme_minimal() +
    labs(x = "Session 1", 
         y = "Session 2",
         title = NULL,
         subtitle = NULL,
         fill = "Correlation") +
    theme(
      axis.text.x = element_text(size = 14, margin = margin(t = 10)),
      axis.text.y = element_text(size = 14, margin = margin(r = 10)),
      axis.title.x = element_text(size = 18, face = "bold", margin = margin(t = 15)),
      axis.title.y = element_text(size = 18, face = "bold", margin = margin(r = 15))
    )
  
  df_name <- deparse(substitute(df))
  cohort_match <- regexpr("cohort_\\d+|cohort\\d+", df_name, ignore.case = TRUE)
  
  if (cohort_match > 0) {
    cohort <- substring(df_name, cohort_match, cohort_match + attr(cohort_match, "match.length") - 1)
    # Standardize format to "cohort#" (no underscore)
    cohort <- gsub("cohort_", "cohort", cohort, ignore.case = TRUE)
  } else {
    cohort <- NULL
  }
  
  # Print to console and save to file if requested
  print(p)
  
  if (save_to_file) {
    # Include cohort in filename if extracted
    if (!is.null(cohort)) {
      filename <- paste0(filename_prefix, "_", cohort, "_", task, ".png")
    } else {
      filename <- paste0(filename_prefix, "_", task, ".png")
    }
    ggsave(filename, plot = p, width = 8, height = 6, dpi = 300)
    cat("Plot saved to", filename, "\n")
  }
  
  return(p)
}
sicr_internal_reliability <- function(df1, df2 = NULL, trial_df = NULL, check.keys = FALSE, digits = 3) {
  # Load required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("This function requires the ggplot2 package. Please install it with install.packages('ggplot2')")
  }
  
  # Determine task type based on column names
  if ("correct_serial_syllables" %in% colnames(df1)) {
    task_type <- "ssl"
    score_types <- c("correct_serial_syllables", "correct_bigrams", 
                     "correct_trigrams", "correct_total_syllables")
    task_name_base <- "SSL"
    trial_task_label <- "Syllable SL"
  } else if ("correct_serial_letters" %in% colnames(df1)) {
    task_type <- "lsl"
    score_types <- c("correct_serial_letters", "correct_bigrams", 
                     "correct_trigrams", "correct_total_letters")
    task_name_base <- "LSL"
    trial_task_label <- "Letter SL"
  } else {
    stop("Input dataframe does not contain expected scoring columns.")
  }
  
  # Determine if we're doing test-retest or internal reliability
  is_test_retest <- !is.null(df2)
  
  # Initialize results data frame
  if (is_test_retest) {
    results <- data.frame(
      Task = c(paste(task_name_base, "Session 1"), 
               paste(task_name_base, "Session 2")),
      stringsAsFactors = FALSE
    )
  } else {
    results <- data.frame(
      Task = paste(task_name_base, "Single Session"),
      stringsAsFactors = FALSE
    )
  }
  
  # Add score type columns
  for (score_type in score_types) {
    results[[score_type]] <- numeric(ifelse(is_test_retest, 2, 1))
  }
  
  # List of dataframes to analyze
  dfs <- if (is_test_retest) list(df1, df2) else list(df1)
  
  # Calculate Cronbach's alpha for each dataframe and score type
  for (i in 1:length(dfs)) {
    df <- dfs[[i]]
    
    # For each score type
    for (score_type in score_types) {
      # Reshape data to wide format for reliability analysis
      wide_data <- tryCatch({
        reshape(
          data = df[, c("id", score_type, "trial_number")],
          idvar = "id",
          timevar = "trial_number",
          direction = "wide"
        )
      }, error = function(e) {
        warning(paste("Error reshaping data for", score_type, "in session", i, ":", e$message))
        return(NULL)
      })
      
      # Skip if reshaping failed
      if (is.null(wide_data)) {
        next
      }
      
      # Verify wide_data has enough columns
      if (ncol(wide_data) <= 1) {
        warning(paste("Insufficient data for", score_type, "in session", i))
        next
      }
      
      # Remove participant_id column for alpha calculation
      reliability_data <- wide_data[, -1]
      
      # Additional diagnostic check
      if (ncol(reliability_data) < 2) {
        warning(paste("Not enough trials to calculate alpha for", score_type, "in session", i))
        next
      }
      
      # Calculate Cronbach's alpha with check.keys parameter
      alpha_result <- tryCatch({
        psych::alpha(reliability_data, check.keys = check.keys)
      }, error = function(e) {
        warning(paste("Alpha calculation error for", score_type, "in session", i, ":", e$message))
        return(NULL)
      })
      
      # Store the result if calculation was successful
      if (!is.null(alpha_result)) {
        results[i, score_type] <- alpha_result$total$raw_alpha
      }
    }
  }
  
  # Return the data frame invisibly
  invisible(results)
}
afc_internal_reliability <- function(trial_df, task_label = NULL, check.keys = FALSE, digits = 3) {
  # Load required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("This function requires the ggplot2 package. Please install it with install.packages('ggplot2')")
  }
  library(tidyr)
  library(dplyr)
  library(psych)
  
  # If no task_label provided, show available options
  if (is.null(task_label)) {
    unique_task_labels <- unique(trial_df$task_label)
    stop("Please specify a task_label. Available options are: ", 
         paste(unique_task_labels, collapse = ", "))
  }
  
  # Validate task_label
  if (!task_label %in% unique(trial_df$task_label)) {
    unique_task_labels <- unique(trial_df$task_label)
    stop("Invalid task_label. Available options are: ", 
         paste(unique_task_labels, collapse = ", "))
  }
  
  # Extract cohort information from dataframe if available
  if ("cohort" %in% colnames(trial_df)) {
    cohort_values <- unique(trial_df$cohort)
    if (length(cohort_values) == 1) {
      cohort_info <- paste0("cohort", cohort_values)
    } else {
      cohort_info <- "cohort_multiple"
    }
  } else {
    # Try to extract from dataframe name using substitute similar to sicr function
    df_name <- deparse(substitute(trial_df))
    cohort_match <- regexpr("cohort\\d+", df_name)
    if (cohort_match != -1) {
      cohort_info <- substr(df_name, cohort_match, cohort_match + attr(cohort_match, "match.length") - 1)
    } else {
      cohort_info <- "cohort_unknown"
    }
  }
  
  # Initialize results data frame
  results <- data.frame(
    Task = c("Session 1", "Session 2"),
    stringsAsFactors = FALSE
  )
  
  # Add column for trial-level accuracy
  results[["Trial-Level Accuracy"]] <- numeric(2)
  results[["Sample Size"]] <- integer(2)
  
  # Iterate through sessions
  for (session in 1:2) {
    # Filter data for specific task and session
    task_trial_df <- trial_df[trial_df$task_label == task_label & 
                                trial_df$session_number == as.character(session), ]
    
    # Diagnostic print
    cat("Session", session, "Diagnostic Information:\n")
    cat("Total rows:", nrow(task_trial_df), "\n")
    cat("Unique participants:", length(unique(task_trial_df$id)), "\n")
    cat("Unique trials:", length(unique(task_trial_df$Trial.Number)), "\n")
    
    # Pivot the data to wide format
    wide_trial_data <- task_trial_df %>%
      dplyr::select(id, Trial.Number, score) %>%
      tidyr::pivot_wider(
        id_cols = id, 
        names_from = Trial.Number, 
        values_from = score, 
        names_prefix = "trial_"
      )
    
    # Store sample size
    results[session, "Sample Size"] <- nrow(wide_trial_data)
    
    # Prepare data for alpha calculation
    # Manually convert to numeric, handling potential list columns
    reliability_data <- wide_trial_data %>%
      dplyr::select(-id) %>%
      dplyr::mutate(dplyr::across(everything(), ~as.numeric(as.character(.))))
    
    # Print column types to diagnose any remaining issues
    cat("\nColumn Types:\n")
    print(sapply(reliability_data, class))
    
    # Calculate Cronbach's alpha
    if (ncol(reliability_data) > 1) {
      tryCatch({
        # Attempt alpha calculation
        trial_alpha_result <- psych::alpha(reliability_data, check.keys = check.keys)
        
        # Store trial-level accuracy reliability
        results[session, "Trial-Level Accuracy"] <- round(trial_alpha_result$total$raw_alpha, digits)
        
        # Print detailed alpha results
        cat("\nAlpha Calculation Results:\n")
        print(trial_alpha_result$total)
      }, error = function(e) {
        warning(paste("Alpha calculation error for session", session, ":", e$message))
        
        # Additional diagnostic information
        cat("\nDiagnostic Information:\n")
        cat("Reliability Data Structure:\n")
        print(str(reliability_data))
        
        # Print unique values in each column
        cat("\nUnique Values in Each Column:\n")
        for (col in names(reliability_data)) {
          cat(col, ":\n")
          print(unique(reliability_data[[col]]))
          cat("\n")
        }
      })
    } else {
      warning(paste("Not enough trials to calculate alpha for session", session))
    }
    
    cat("\n")
  }
  
  # Determine task type abbreviation from task_label
  task_type <- "unknown"
  if (grepl("Syllable", task_label, ignore.case = TRUE)) {
    task_type <- "ssl"
  } else if (grepl("Letter", task_label, ignore.case = TRUE)) {
    task_type <- "lsl"
  }
  
  # Set file name and path similar to sicr_internal_reliability function
  filename <- paste0("internal_reliability_", cohort_info, "_", task_type, "_2afc.png")
  output_dir <- "~/wd_manuscript/plots/reliability"
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_path <- file.path(output_dir, filename)
  
  # Create title for the plot
  title <- paste("Cronbach's Alpha for", task_label, "2AFC Task")
  
  # Create a copy of results without the Sample Size column for the plot
  plot_results <- results[, c("Task", "Trial-Level Accuracy")]
  
  return(results)
}
combined_internal_reliability <- function(..., check.keys = TRUE, digits = 3) {
  # Load required packages
  required_packages <- c("ggplot2", "tidyr", "dplyr", "gridExtra", "grid")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste("This function requires the", pkg, "package. Please install it with install.packages('", pkg, "')"))
    }
  }
  
  # Get the list of dataframes
  df_list <- list(...)
  
  # Get names of dataframes
  df_names <- as.character(match.call())[-1]
  df_names <- df_names[!df_names %in% c("check.keys", "digits")]
  
  # Initialize results data frame
  results <- data.frame(
    Task = character(),
    Score_Type = character(),
    Cohort1_Session1 = numeric(),
    Cohort1_Session2 = numeric(),
    Cohort2_Session1 = numeric(),
    Cohort2_Session2 = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Process each dataframe
  for (i in 1:length(df_list)) {
    df <- df_list[[i]]
    df_name <- df_names[i]
    
    # Extract cohort information from dataframe name
    cohort_match <- regexpr("cohort\\d+", df_name)
    if (cohort_match != -1) {
      cohort <- substr(df_name, cohort_match, cohort_match + attr(cohort_match, "match.length") - 1)
      cohort_num <- as.numeric(gsub("cohort", "", cohort))
    } else {
      cohort <- "cohort_unknown"
      cohort_num <- NA
    }
    
    # Extract session information from dataframe name
    session_match <- regexpr("_s\\d+", df_name)
    if (session_match != -1) {
      session <- substr(df_name, session_match + 2, session_match + attr(session_match, "match.length") - 1)
      session_num <- as.numeric(session)
    } else {
      session_num <- NA
    }
    
    # Check if it's a SICR or AFC dataframe
    if (grepl("sicr", df_name)) {
      # Process SICR dataframe
      task_type <- ifelse(grepl("ssl", df_name), "SSL", "LSL")
      
      # Determine task type for column selection
      if (grepl("ssl", df_name)) {
        score_types <- c("correct_serial_syllables", "correct_bigrams", 
                         "correct_trigrams", "correct_total_syllables")
      } else {
        score_types <- c("correct_serial_letters", "correct_bigrams", 
                         "correct_trigrams", "correct_total_letters")
      }
      
      # Get reliability results
      rel_results <- sicr_internal_reliability(df, check.keys = check.keys, digits = digits)
      
      # Process results for each score type
      for (score_type in score_types) {
        # Check if this combination already exists in results
        task_row <- which(results$Task == task_type & results$Score_Type == score_type)
        
        if (length(task_row) == 0) {
          # Create new row
          new_row <- data.frame(
            Task = task_type,
            Score_Type = score_type,
            Cohort1_Session1 = NA,
            Cohort1_Session2 = NA,
            Cohort2_Session1 = NA,
            Cohort2_Session2 = NA,
            stringsAsFactors = FALSE
          )
          results <- rbind(results, new_row)
          task_row <- nrow(results)
        }
        
        # Column name based on cohort and session
        col_name <- paste0("Cohort", cohort_num, "_Session", session_num)
        
        # Update the appropriate column
        if (cohort_num == 1 && session_num == 1) {
          results[task_row, "Cohort1_Session1"] <- rel_results[[score_type]]
        } else if (cohort_num == 1 && session_num == 2) {
          results[task_row, "Cohort1_Session2"] <- rel_results[[score_type]]
        } else if (cohort_num == 2 && session_num == 1) {
          results[task_row, "Cohort2_Session1"] <- rel_results[[score_type]]
        } else if (cohort_num == 2 && session_num == 2) {
          results[task_row, "Cohort2_Session2"] <- rel_results[[score_type]]
        }
      }
    } else if (grepl("afc", df_name)) {
      # Process AFC dataframe
      # Determine task type from dataframe content
      if (is.null(df) || nrow(df) == 0) {
        warning(paste("Empty dataframe:", df_name))
        next
      }
      
      # Try to determine task types from task_label column
      task_labels <- unique(df$task_label)
      
      for (task_label in task_labels) {
        # Skip if not a relevant SL task
        if (!grepl("Syllable|Letter", task_label, ignore.case = TRUE)) {
          next
        }
        
        # Determine if SSL or LSL
        task_type <- ifelse(grepl("Syllable", task_label, ignore.case = TRUE), "SSL", "LSL")
        
        # Get reliability results for this task_label
        rel_results <- afc_internal_reliability(df, task_label = task_label, check.keys = check.keys, digits = digits)
        
        # Process results for trial-level accuracy
        score_type <- "Trial-Level Accuracy"
        
        # Check if this combination already exists in results
        task_row <- which(results$Task == paste0(task_type, " 2AFC") & results$Score_Type == score_type)
        
        if (length(task_row) == 0) {
          # Create new row
          new_row <- data.frame(
            Task = paste0(task_type, " 2AFC"),
            Score_Type = score_type,
            Cohort1_Session1 = NA,
            Cohort1_Session2 = NA,
            Cohort2_Session1 = NA,
            Cohort2_Session2 = NA,
            stringsAsFactors = FALSE
          )
          results <- rbind(results, new_row)
          task_row <- nrow(results)
        }
        
        # Update both session columns since AFC data includes both sessions
        if (cohort_num == 1) {
          results[task_row, "Cohort1_Session1"] <- rel_results[1, "Trial-Level Accuracy"]
          results[task_row, "Cohort1_Session2"] <- rel_results[2, "Trial-Level Accuracy"]
        } else if (cohort_num == 2) {
          results[task_row, "Cohort2_Session1"] <- rel_results[1, "Trial-Level Accuracy"]
          results[task_row, "Cohort2_Session2"] <- rel_results[2, "Trial-Level Accuracy"]
        }
      }
    }
  }
  
  # Round all numeric columns to the specified number of digits
  numeric_cols <- c("Cohort1_Session1", "Cohort1_Session2", "Cohort2_Session1", "Cohort2_Session2")
  results[numeric_cols] <- round(results[numeric_cols], digits)
  
  # Create a better display name for score types
  display_names <- c(
    "correct_serial_syllables" = "Serial Syllables",
    "correct_serial_letters" = "Serial Letters",
    "correct_bigrams" = "Bigrams",
    "correct_trigrams" = "Trigrams",
    "correct_total_syllables" = "Total Syllables",
    "correct_total_letters" = "Total Letters",
    "Trial-Level Accuracy" = "Trial-Level Accuracy"
  )
  
  # Replace score type codes with human-readable names
  results$Score_Type <- sapply(results$Score_Type, function(x) {
    if (x %in% names(display_names)) display_names[x] else x
  })
  
  # Create output directory if it doesn't exist
  output_dir <- "~/wd_manuscript/plots/reliability"
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_path <- file.path(output_dir, "internal_reliability.png")
  
  # Create title for the plot
  title <- "Internal Reliability (Cronbach's Alpha) Across Tasks, Score Types, Cohorts, and Sessions"
  
  # Create a themed table for the plot
  table_grob <- gridExtra::tableGrob(
    results, 
    rows = NULL,
    theme = gridExtra::ttheme_minimal(
      core = list(fg_params = list(hjust = 0.5, x = 0.5, fontsize = 9)),
      colhead = list(fg_params = list(hjust = 0.5, x = 0.5, fontsize = 10, fontface = "bold")),
      rowhead = list(fg_params = list(hjust = 0, x = 0, fontsize = 9))
    )
  )
  
  # Save the plot as PNG without using Chrome
  tryCatch({
    png(output_path, width = 1000, height = 800, res = 120)
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 1, heights = c(0.1, 0.9))))
    grid::pushViewport(grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
    grid::grid.text(title, gp = grid::gpar(fontsize = 14, fontface = "bold"))
    grid::popViewport()
    grid::pushViewport(grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
    grid::grid.draw(table_grob)
    grid::popViewport(2)
    dev.off()
    message("Table saved as PNG to: ", output_path)
  }, error = function(e) {
    warning("Could not save PNG: ", e$message)
    
    # Alternative method using ggplot2
    tryCatch({
      p <- ggplot2::ggplot() + 
        ggplot2::theme_void() +
        ggplot2::labs(title = title) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.margin = ggplot2::unit(c(1, 1, 1, 1), "cm")
        )
      
      ggplot2::ggsave(
        filename = output_path,
        plot = gridExtra::grid.arrange(
          p, 
          table_grob,
          heights = c(0.1, 0.9)
        ),
        width = 10, 
        height = 8,
        dpi = 120
      )
      message("Table saved as PNG using ggplot2 to: ", output_path)
    }, error = function(e2) {
      warning("Could not save PNG using alternative method: ", e2$message)
    })
  })
  
  # Print the results
  cat("\nInternal Reliability Results\n\n")
  print(results, digits = digits)
  
  # Return the results
  return(results)
}

# computes correlations and partial correlations; produces scatter plot visualizations
compute_correlation <- function(df, independent, dependent) {
  
  # Function to generate labels dynamically
  generate_label <- function(variable_name) {
    # Check if the task is PSRT, SSL, or an assessment
    if (str_detect(variable_name, "psrt")) {
      task <- "PSRT"
      session <- str_extract(variable_name, "s[0-9]+") # Extract session (e.g., s1, s2)
      target <- ifelse(str_detect(variable_name, "_1_") | str_detect(variable_name, "_1$"), "Target", "Foil")
      feature <- case_when(
        str_detect(variable_name, "bigrams") ~ "Correct Bigrams",
        str_detect(variable_name, "serial_syllables") ~ "Correct Serial Syllables",
        str_detect(variable_name, "syllables_total") ~ "Correct Total Syllables",
        str_detect(variable_name, "trigrams") ~ "Correct Trigrams",
        str_detect(variable_name, "diff_big") ~ "Bigram Difference Score",
        str_detect(variable_name, "diff_syl") ~ "Syllable Difference Score",
        TRUE ~ "Unknown Feature"
      )
      paste(task, toupper(paste0("(", session, ")")), ":", feature, "(", target, ")")
    } else if (str_detect(variable_name, "ssl")) {
      task <- "SSL"
      target <- ifelse(str_detect(variable_name, "_1_") | str_detect(variable_name, "_1$"), "Target", "Foil")
      feature <- case_when(
        str_detect(variable_name, "bigrams") ~ "Correct Bigrams",
        str_detect(variable_name, "serial_syllables") ~ "Correct Serial Syllables",
        str_detect(variable_name, "syllables_total") ~ "Correct Total Syllables",
        str_detect(variable_name, "trigrams") ~ "Correct Trigrams",
        str_detect(variable_name, "diff_big") ~ "Bigram Difference Score",
        str_detect(variable_name, "diff_syl") ~ "Syllable Difference Score",
        TRUE ~ "Unknown Feature"
      )
      paste(task, ":", feature, "(", target, ")")
    } else {
      # Handle other assessments
      assessment_label <- case_when(
        variable_name == "sentence_comp_score" ~ "Sentence Comprehension Score",
        variable_name == "nwrep_raw_score" ~ "Non-Word Repetition Raw Score",
        variable_name == "nwrep_standard_score" ~ "Non-Word Repetition Standard Score",
        variable_name == "sentence_rep_raw_score" ~ "Redmond Sentence Recall Raw Score",
        variable_name == "sentence_rep_standard_score" ~ "Redmond Sentence Recall Standard Score",
        TRUE ~ "Unknown Assessment"
      )
      assessment_label
    }
  }
  
  
  # Subset the data (only necessary columns)
  df_subset <- df %>%
    mutate(across(all_of(c(independent, dependent)), ~ as.numeric(.), .names = "{.col}")) %>%
    dplyr::select(all_of(c(independent, dependent))) %>%
    tidyr::drop_na()  # Remove rows with missing data
  
  # Ensure there are enough observations
  if (nrow(df_subset) > 6) {
    
    # Print the formula being used
    cat("\n------------------------------------------\n")
    cat("Computing Partial Correlation:\n")
    cat("ρ(", independent, ",", dependent, ")\n\n")
    
    # Run partial correlation test
    cor_result <- cor.test(df_subset[[independent]], df_subset[[dependent]], method = "pearson")
    
    # Extract correlation coefficient and p-value
    r_value <- round(cor_result$estimate, 3)
    p_value <- signif(cor_result$p.value, 3)
    
    # Dynamically generate plot labels
    independent_label <- generate_label(independent)
    dependent_label <- generate_label(dependent)
    
    # Create scatter plot with correlation results
    ggplot(df_subset, aes(x = .data[[independent]], y = .data[[dependent]])) +
      geom_point(color = "deeppink", size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", color = "lightpink", se = FALSE) +
      labs(
        x = independent_label,
        y = dependent_label,
        title = paste(independent_label, "vs.", dependent_label),
        subtitle = paste("R =", r_value, "| p =", p_value)
      ) +
      theme_minimal(base_family = "Times New Roman")
    
  } else {
    cat("\nNot enough data for partial correlation between", independent, "and", dependent, "\n")
  }
}
# computes partial correlations for relationships between ssl and assessments (concurrent & post-intervention gains) with plotting
compute_partial_correlation <- function(df, independent, dependent, control_vars, cohort = "cohort1") {
  # Load necessary libraries if not already loaded
  if (!requireNamespace("dplyr", quietly = TRUE)) library(dplyr)
  if (!requireNamespace("tidyr", quietly = TRUE)) library(tidyr)
  if (!requireNamespace("ggplot2", quietly = TRUE)) library(ggplot2)
  if (!requireNamespace("stringr", quietly = TRUE)) library(stringr)
  
  # Set colors based on cohort
  if (cohort == "cohort1") {
    point_color <- "#63278b"  # Original purple
    line_color <- "#d5bae6ff" # Original light purple
  } else {
    point_color <- "deeppink3"
    line_color <- "lightpink1"
  }
  
  # Function to generate labels dynamically
  generate_label <- function(variable_name) {
    # Check for 2AFC specifically in SSL and LSL tasks
    if ((stringr::str_detect(variable_name, "ssl") || stringr::str_detect(variable_name, "lsl")) && 
        stringr::str_detect(variable_name, "afc")) {
      return("2AFC Accuracy")
    }
    
    # Then check other task types
    if (stringr::str_detect(variable_name, "ssl")) {
      task <- "SSL"
      target <- ifelse(stringr::str_detect(variable_name, "_1_") | stringr::str_detect(variable_name, "_1$"), "Target", "Foil")
      feature <- dplyr::case_when(
        stringr::str_detect(variable_name, "bigrams") ~ "Correct Bigrams",
        stringr::str_detect(variable_name, "serial_syllables") ~ "Correct Serial Syllables",
        stringr::str_detect(variable_name, "syllables_total") ~ "Correct Total Syllables",
        stringr::str_detect(variable_name, "trigrams") ~ "Correct Trigrams",
        stringr::str_detect(variable_name, "diff_big") ~ "Bigram Difference Score",
        stringr::str_detect(variable_name, "diff_syl") ~ "Syllable Difference Score",
        TRUE ~ "Unknown Feature"
      )
      return(paste(task, ":", feature, "(", target, ")"))
    } else if (stringr::str_detect(variable_name, "lsl")) {
      task <- "LSL"
      target <- ifelse(stringr::str_detect(variable_name, "_1_") | stringr::str_detect(variable_name, "_1$"), "Target", "Foil")
      feature <- dplyr::case_when(
        stringr::str_detect(variable_name, "bigrams") ~ "Correct Bigrams",
        stringr::str_detect(variable_name, "serial_letters") ~ "Correct Serial Letters",
        stringr::str_detect(variable_name, "letters_total") ~ "Correct Total Letters",
        stringr::str_detect(variable_name, "trigrams") ~ "Correct Trigrams",
        stringr::str_detect(variable_name, "diff_big") ~ "Bigram Difference Score",
        stringr::str_detect(variable_name, "diff_syl") ~ "Letter Difference Score",
        TRUE ~ "Unknown Feature"
      )
      return(paste(task, ":", feature, "(", target, ")"))
    } else {
      # Handle other assessments
      assessment_label <- dplyr::case_when(
        stringr::str_detect(variable_name, "taps_deletion_ss") ~ ifelse(stringr::str_detect(variable_name, "_post"), "TAPS Phoneme Deletion (Post)", "TAPS Phoneme Deletion (Pre)"),
        stringr::str_detect(variable_name, "dibels_nwf_cls") ~ ifelse(stringr::str_detect(variable_name, "_post"), "DIBELS NWF Correct Letter Sounds (Post)", "DIBELS NWF Correct Letter Sounds (Pre)"),
        stringr::str_detect(variable_name, "dibels_nwf_wrc") ~ ifelse(stringr::str_detect(variable_name, "_post"), "DIBELS NWF Words Read Correctly (Post)", "DIBELS NWF Words Read Correctly (Pre)"),
        stringr::str_detect(variable_name, "dibels_orf") ~ ifelse(stringr::str_detect(variable_name, "_post"), "DIBELS Oral Reading Fluency (Post)", "DIBELS Oral Reading Fluency (Pre)"),
        stringr::str_detect(variable_name, "towre_swe_ss") ~ ifelse(stringr::str_detect(variable_name, "_post"), "TOWRE Sight Word Efficiency (Post)", "TOWRE Sight Word Efficiency (Pre)"),
        stringr::str_detect(variable_name, "towre_pde_ss") ~ ifelse(stringr::str_detect(variable_name, "_post"), "TOWRE Phonemic Decoding Efficiency (Post)", "TOWRE Phonemic Decoding Efficiency (Pre)"),
        variable_name == "sentence_comp_score" ~ "Sentence Comprehension Score",
        variable_name == "nwrep_raw_score" ~ "Non-Word Repetition Raw Score",
        variable_name == "nwrep_standard_score" ~ "Non-Word Repetition Standard Score",
        variable_name == "sentence_rep_raw_score" ~ "Redmond Sentence Recall Raw Score",
        variable_name == "sentence_rep_standard_score" ~ "Redmond Sentence Recall Standard Score",
        TRUE ~ variable_name
      )
      return(assessment_label)
    }
  }
  
  # Handle input that can be either a single variable or vector of variables
  if (length(control_vars) == 1 && is.character(control_vars)) {
    control_vars <- c(control_vars)
  }
  
  # Create the list of all variables
  all_vars <- c(independent, dependent, control_vars)
  
  # Subset the data (only necessary columns)
  df_subset <- df %>%
    dplyr::select(dplyr::all_of(all_vars)) %>%
    tidyr::drop_na()  # Remove rows with missing data
  
  # Ensure there are enough observations
  if (nrow(df_subset) > 2) {
    # Print the formula being used
    cat("\n------------------------------------------\n")
    cat("Computing Partial Correlation:\n")
    cat("ρ(", independent, ",", dependent, "|", paste(control_vars, collapse=", "), ")\n\n")
    
    # Run partial correlation test
    if (length(control_vars) == 1) {
      # Single control variable
      partial_result <- pcor.test(df_subset[[independent]], df_subset[[dependent]], df_subset[[control_vars]], method = "pearson")
    } else {
      # Multiple control variables
      control_matrix <- as.matrix(df_subset[, control_vars])
      partial_result <- pcor.test(df_subset[[independent]], df_subset[[dependent]], control_matrix, method = "pearson")
    }
    
    # Extract correlation coefficient and p-value
    r_value <- round(partial_result$estimate, 3)
    p_value <- signif(partial_result$p.value, 3)
    
    # Generate labels for variables
    independent_label <- generate_label(independent)
    dependent_label <- generate_label(dependent)
    control_labels <- sapply(control_vars, generate_label)
    control_label_text <- paste(control_labels, collapse=", ")
    
    # Create plot with variable labels but no font family specification
    p <- ggplot2::ggplot(df_subset, aes(x = .data[[independent]], y = .data[[dependent]])) +
      ggplot2::geom_point(color = point_color, size = 3, alpha = 0.7) +
      ggplot2::geom_smooth(method = "lm", color = line_color, se = FALSE) +
      ggplot2::labs(
        x = independent_label,
        y = dependent_label,
        title = paste(independent_label, "vs.", dependent_label),
        subtitle = paste("Controlled for:", control_label_text),
        caption = paste("Partial correlation: R =", r_value, "| p =", p_value, "| n =", nrow(df_subset))
      ) +
      ggplot2::theme_minimal() + 
      ggplot2::theme(
        text = ggplot2::element_text(family = ""),
        plot.title = ggplot2::element_text(family = ""),
        axis.title = ggplot2::element_text(family = ""),
        axis.text = ggplot2::element_text(family = "")
      )
    
    return(p)
  } else {
    cat("\nNot enough data for partial correlation between", independent, "and", dependent, "\n")
    # Return an empty plot
    p <- ggplot2::ggplot() + ggplot2::theme_void()
    return(p)
  }
}
# runs all necessary ssl correlations; plots & saves
run_ssl_partial_correlations <- function(df, significance_threshold = 0.1) {
  # Get the dataframe name for file naming
  df_name <- deparse(substitute(df))
  cohort <- ""
  if (grepl("cohort_1", df_name)) {
    cohort <- "cohort1"
    cohort_suffix <- "cohort1_"
  } else if (grepl("cohort_2", df_name)) {
    cohort <- "cohort2"
    cohort_suffix <- "cohort2_"
  }
  
  # Create task-specific output directory
  output_dir <- paste0("~/wd_manuscript/plots/correlations/", cohort, "_ssl")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Lists to store significant results
  significant_plots <- list()
  significant_results <- data.frame(
    Independent = character(),
    Dependent = character(), 
    Control = character(),
    R_value = numeric(),
    P_value = numeric(),
    N = integer(),
    stringsAsFactors = FALSE
  )
  
  # Identify SSL accuracy variables
  ssl_acc_vars <- names(df)[grep("afc_accuracy_ssl", names(df))]
  
  # Identify SSL performance variables for targets (with _1_ in name)
  ssl_perf_target_vars <- names(df)[grep("correct_.*\\.ssl_1_", names(df))]
  if (length(ssl_perf_target_vars) == 0) {
    # Try alternative pattern if first one fails
    ssl_perf_target_vars <- names(df)[grep("correct_.*_ssl_1_", names(df))]
  }
  
  # Identify assessment variables
  assessment_pre_vars <- names(df)[grep("(taps|dibels|towre).*_pre$", names(df))]
  assessment_post_vars <- names(df)[grep("(taps|dibels|towre).*_post$", names(df))]
  
  # Identify foil variables (for controlling)
  ssl_perf_foil_vars <- names(df)[grep("correct_.*\\.ssl_0_", names(df))]
  if (length(ssl_perf_foil_vars) == 0) {
    # Try alternative pattern if first one fails
    ssl_perf_foil_vars <- names(df)[grep("correct_.*_ssl_0_", names(df))]
  }
  
  # Counter for plots
  plot_count <- 0
  
  # 1. SSL accuracy vs post-assessments (controlling for pre-assessment)
  cat("Running correlations for SSL accuracy vs post-assessments...\n")
  for (acc_var in ssl_acc_vars) {
    for (i in seq_along(assessment_post_vars)) {
      dependent <- assessment_post_vars[i]
      # Find the corresponding pre-assessment
      control <- gsub("_post$", "_pre", dependent)
      
      # Check if control variable exists in the dataframe
      if (control %in% names(df)) {
        # Calculate first to check significance
        vars_to_select <- c(acc_var, dependent, control)
        df_subset <- df[, vars_to_select, drop = FALSE]
        df_subset <- df_subset[complete.cases(df_subset), , drop = FALSE]
        
        if (nrow(df_subset) > 2) {
          # Compute manually to verify significance
          result <- pcor.test(df_subset[[acc_var]], df_subset[[dependent]], 
                              df_subset[[control]], method = "pearson")
          
          estimate <- result$estimate
          p_value <- result$p.value
          n <- nrow(df_subset)
          
          if (p_value < significance_threshold) {
            # If significant, run the original function to get the plot
            plot <- compute_partial_correlation(df, acc_var, dependent, control, cohort)
            
            plot_count <- plot_count + 1
            significant_plots[[plot_count]] <- plot
            
            # Save individual plot
            # Create clean variable names for filename
            clean_indep <- gsub("\\.", "_", acc_var)
            clean_dep <- gsub("\\.", "_", dependent)
            filename <- paste0(output_dir, "/", clean_indep, "_vs_", clean_dep, ".png")
            
            # Save plot as PNG
            ggplot2::ggsave(filename, plot, width = 7, height = 5, dpi = 300)
            cat("Saved plot to:", filename, "\n")
            
            # Add to results dataframe
            significant_results <- rbind(significant_results, data.frame(
              Independent = acc_var,
              Dependent = dependent,
              Control = control,
              R_value = round(estimate, 3),
              P_value = signif(p_value, 3),
              N = n
            ))
          }
        }
      }
    }
  }
  
  # 2. SSL target performance vs pre-assessments (controlling for foil performance)
  cat("Running correlations for SSL target performance vs pre-assessments...\n")
  
  if (length(ssl_perf_target_vars) > 0 && length(assessment_pre_vars) > 0) {
    for (target_var in ssl_perf_target_vars) {
      # Find the corresponding foil variable to use as control
      # Try different patterns to match the foil variable
      control_var <- NULL
      
      # Extract session info
      session <- stringr::str_extract(target_var, "s[0-9]+$")
      if (!is.na(session)) {
        # Check different patterns
        if (grepl("\\.ssl_1_", target_var)) {
          base_name <- gsub("\\.ssl_1_.*$", "", target_var)
          control_var <- paste0(base_name, ".ssl_0_", session)
        } else if (grepl("_ssl_1_", target_var)) {
          base_name <- gsub("_ssl_1_.*$", "", target_var)
          control_var <- paste0(base_name, "_ssl_0_", session)
        }
      }
      
      # Check if control variable exists
      if (!is.null(control_var) && control_var %in% names(df)) {
        for (pre_var in assessment_pre_vars) {
          # Calculate first to check significance
          vars_to_select <- c(target_var, pre_var, control_var)
          df_subset <- df[, vars_to_select, drop = FALSE]
          df_subset <- df_subset[complete.cases(df_subset), , drop = FALSE]
          
          if (nrow(df_subset) > 2) {
            # Compute manually to verify significance
            result <- pcor.test(df_subset[[target_var]], df_subset[[pre_var]], 
                                df_subset[[control_var]], method = "pearson")
            
            estimate <- result$estimate
            p_value <- result$p.value
            n <- nrow(df_subset)
            
            if (p_value < significance_threshold) {
              # If significant, run the original function to get the plot
              plot <- compute_partial_correlation(df, target_var, pre_var, control_var, cohort)
              
              plot_count <- plot_count + 1
              significant_plots[[plot_count]] <- plot
              
              # Save individual plot
              # Create clean variable names for filename
              clean_indep <- gsub("\\.", "_", target_var)
              clean_dep <- gsub("\\.", "_", pre_var)
              filename <- paste0(output_dir, "/", clean_indep, "_vs_", clean_dep, ".png")
              
              # Save plot as PNG
              ggplot2::ggsave(filename, plot, width = 7, height = 5, dpi = 300)
              cat("Saved plot to:", filename, "\n")
              
              # Add to results dataframe
              significant_results <- rbind(significant_results, data.frame(
                Independent = target_var,
                Dependent = pre_var,
                Control = control_var,
                R_value = round(estimate, 3),
                P_value = signif(p_value, 3),
                N = n
              ))
            }
          }
        }
      }
    }
  }
  
  # Sort results by p-value
  if (nrow(significant_results) > 0) {
    significant_results <- significant_results[order(significant_results$P_value), ]
    
    # Combine plots into a grid if there are any
    if (length(significant_plots) > 0) {
      # Determine grid layout based on number of plots
      n_plots <- length(significant_plots)
      n_cols <- min(3, n_plots)  # Maximum 3 columns
      n_rows <- ceiling(n_plots / n_cols)
      
      combined_plot <- patchwork::wrap_plots(significant_plots, ncol = n_cols)
      
      # Save the grid plot
      grid_filename <- paste0(output_dir, "/ssl_correlation_grid.png")
      ggplot2::ggsave(grid_filename, combined_plot, width = min(n_cols * 7, 21), height = n_rows * 5, dpi = 300)
      cat("Saved correlation grid to:", grid_filename, "\n")
      
      return(list(plot = combined_plot, results = significant_results))
    } else {
      return(list(results = significant_results))
    }
  } else {
    return(NULL)
  }
}

# create afc & lmer visualizations
plot_cohort1_ssl_accuracy <- function(cohort1_afc) {
  # Reshape data for plotting (only SSL task)
  plot_data <- cohort1_afc %>%
    dplyr::select(id, contains("ssl")) %>%
    pivot_longer(
      cols = contains("ssl"),
      names_to = "task_session",
      values_to = "afc_accuracy"
    ) %>%
    # Extract session number and map to intervention labels
    mutate(
      session_number = as.numeric(substr(task_session, nchar(task_session), nchar(task_session))),
      session = factor(ifelse(session_number == 1, "Pre-intervention", "Post-intervention"), 
                       levels = c("Pre-intervention", "Post-intervention"))
    )
  
  # Calculate summary statistics for plotting
  summary_data <- plot_data %>%
    group_by(session) %>%
    summarize(
      mean_accuracy = mean(afc_accuracy, na.rm = TRUE),
      se = sd(afc_accuracy, na.rm = TRUE) / sqrt(n()),
      n = n(),
      # T-test against chance level (0.5)
      p_value = t.test(afc_accuracy, mu = 0.5)$p.value,
      # Create significance stars
      stars = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01 ~ "**", 
        p_value < 0.05 ~ "*",
        TRUE ~ "ns"
      ),
      .groups = "drop"
    )
  
  p <- ggplot(summary_data, aes(x = session, y = mean_accuracy, fill = session)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.6) +
    geom_errorbar(aes(ymin = mean_accuracy - se, ymax = mean_accuracy + se), width = 0.2) +
    # Add significance stars (only for significant results)
    geom_text(data = . %>% filter(stars != "ns"),
              aes(x = session,  
                  y = mean_accuracy + se + 0.08,  
                  label = stars),
              size = 15,
              vjust = -0.5) +
    scale_size_identity() +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "darkgray") +
    labs(x = "Session", 
         y = "Mean Accuracy", 
         fill = "Session") +
    scale_fill_manual(values = c("Pre-intervention" = "#d5bae6", "Post-intervention" = "#63278b")) +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 32, face = "bold", margin = margin(t = 20)),
      axis.title.y = element_text(size = 32, face = "bold", margin = margin(r = 20)),
      axis.text = element_text(size = 30),
      legend.position = "none"
    ) +
    ylim(0, 1)  # Set y-axis limit
  
  # Create directory if it doesn't exist
  dir.create("plots", showWarnings = FALSE)
  
  # Save the plot
  plot_file_path <- "~/wd_manuscript/plots/presentation_plots/cohort1_ssl_accuracy.png"
  ggsave(plot_file_path, p, width = 10, height = 8, dpi = 300)
  
  return(p)
}
plot_cohort2_ssl_accuracy <- function(cohort2_afc) {
  # Reshape data for plotting (only SSL task)
  plot_data <- cohort2_afc %>%
    dplyr::select(id, contains("ssl")) %>%
    pivot_longer(
      cols = contains("ssl"),
      names_to = "task_session",
      values_to = "afc_accuracy"
    ) %>%
    # Extract session number and map to intervention labels
    mutate(
      session_number = as.numeric(substr(task_session, nchar(task_session), nchar(task_session))),
      session = factor(ifelse(session_number == 1, "Pre-intervention", "Post-intervention"), 
                       levels = c("Pre-intervention", "Post-intervention"))
    )
  
  # Calculate summary statistics for plotting
  summary_data <- plot_data %>%
    group_by(session) %>%
    summarize(
      mean_accuracy = mean(afc_accuracy, na.rm = TRUE),
      se = sd(afc_accuracy, na.rm = TRUE) / sqrt(n()),
      n = n(),
      # T-test against chance level (0.5)
      p_value = t.test(afc_accuracy, mu = 0.5)$p.value,
      # Create significance stars
      stars = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01 ~ "**", 
        p_value < 0.05 ~ "*",
        TRUE ~ "ns"
      ),
      .groups = "drop"
    )
  
  p <- ggplot(summary_data, aes(x = session, y = mean_accuracy, fill = session)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.6) +
    geom_errorbar(aes(ymin = mean_accuracy - se, ymax = mean_accuracy + se), width = 0.2) +
    # Add significance stars (only for significant results)
    geom_text(data = . %>% filter(stars != "ns"),
              aes(x = session,  
                  y = mean_accuracy + se + 0.08,  
                  label = stars),
              size = 15,
              vjust = -0.5) +
    scale_size_identity() +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "darkgray") +
    labs(x = "Session", 
         y = "Mean Accuracy", 
         fill = "Session") +
    scale_fill_manual(values = c("Pre-intervention" = "#ffc9db", "Post-intervention" = "#981340")) +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 32, face = "bold", margin = margin(t = 20)),
      axis.title.y = element_text(size = 32, face = "bold", margin = margin(r = 20)),
      axis.text = element_text(size = 30),
      legend.position = "none"
    ) +
    ylim(0, 1)  # Set y-axis limit
  
  # Create directory if it doesn't exist
  dir.create("plots", showWarnings = FALSE)
  
  # Save the plot
  plot_file_path <- "~/wd_manuscript/plots/presentation_plots/cohort2_ssl_accuracy.png"
  ggsave(plot_file_path, p, width = 10, height = 8, dpi = 300)
  
  return(p)
}
wd_lmer_test_combined_cohorts_ssl <- function(df_c2_s1, df_c2_s2, df_c1_s2, bar_spacing = 0.8) {
  # Set the multiplier for adjusting the space above the highest bar
  star_distance_multiplier <- 0.5
  
  # Update to use correct column names for SSL task with .ssl suffix
  bigram_col <- "correct_bigrams.ssl"
  
  # Ensure required columns exist in all dataframes
  for (df in list(df_c2_s1, df_c2_s2, df_c1_s2)) {
    if(!(bigram_col %in% names(df))) {
      stop("Required bigram score column not found in one of the dataframes.")
    }
  }
  
  # Add session and cohort identifiers to each dataframe
  df_c2_s1$session <- "Session 1"
  df_c2_s1$cohort <- "Cohort 2"
  
  df_c2_s2$session <- "Session 2"
  df_c2_s2$cohort <- "Cohort 2"
  
  df_c1_s2$session <- "Session 2"
  df_c1_s2$cohort <- "Cohort 1"
  
  # Combine dataframes
  combined_df <- rbind(df_c2_s1, df_c2_s2, df_c1_s2)
  
  # Prepare data for analysis - using only bigram scores
  long_df <- combined_df %>%
    dplyr::select(id, target_tag, session, cohort, !!sym(bigram_col)) %>%
    dplyr::rename(score = !!sym(bigram_col)) %>%
    mutate(score_type = "bigram")
  
  # Create labels for plotting
  condition_labels <- c("0" = "Foil", "1" = "Target")
  long_df <- long_df %>%
    mutate(condition = factor(target_tag),
           condition_label = factor(condition_labels[as.character(target_tag)]),
           session = factor(session, levels = c("Session 1", "Session 2")),
           cohort = factor(cohort, levels = c("Cohort 1", "Cohort 2")),
           # Create a combined factor for session and cohort
           cohort_session = interaction(cohort, session, sep = "_"))
  
  # Run LMER tests
  print("Running linear mixed effects models for SSL bigram scores...")
  
  # Create a list to store results
  results_list <- list()
  
  # Run model for Cohort 2, Session 1
  c2_s1_data <- filter(long_df, cohort == "Cohort 2" & session == "Session 1")
  c2_s1_model <- lmer(score ~ condition + (1|id), data = c2_s1_data)
  c2_s1_summary <- summary(c2_s1_model)
  
  # Run model for Cohort 2, Session 2
  c2_s2_data <- filter(long_df, cohort == "Cohort 2" & session == "Session 2")
  c2_s2_model <- lmer(score ~ condition + (1|id), data = c2_s2_data)
  c2_s2_summary <- summary(c2_s2_model)
  
  # Run model for Cohort 1, Session 2
  c1_s2_data <- filter(long_df, cohort == "Cohort 1" & session == "Session 2")
  c1_s2_model <- lmer(score ~ condition + (1|id), data = c1_s2_data)
  c1_s2_summary <- summary(c1_s2_model)
  
  # Store results
  results_list[["Cohort 2_Session 1"]] <- list(
    bigram = list(
      model = c2_s1_model,
      summary = c2_s1_summary,
      p_value = round(c2_s1_summary$coefficients[2, "Pr(>|t|)"], 4),
      effect = round(c2_s1_summary$coefficients[2, "Estimate"], 4)
    )
  )
  
  results_list[["Cohort 2_Session 2"]] <- list(
    bigram = list(
      model = c2_s2_model,
      summary = c2_s2_summary,
      p_value = round(c2_s2_summary$coefficients[2, "Pr(>|t|)"], 4),
      effect = round(c2_s2_summary$coefficients[2, "Estimate"], 4)
    )
  )
  
  results_list[["Cohort 1_Session 2"]] <- list(
    bigram = list(
      model = c1_s2_model,
      summary = c1_s2_summary,
      p_value = round(c1_s2_summary$coefficients[2, "Pr(>|t|)"], 4),
      effect = round(c1_s2_summary$coefficients[2, "Estimate"], 4)
    )
  )
  
  # Calculate means and standard errors for plotting
  df_summary <- long_df %>%
    group_by(cohort, session, condition, condition_label) %>%
    summarize(
      mean_score = mean(score, na.rm = TRUE),
      se = sd(score, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
  
  # Create a custom order for the x-axis
  df_summary <- df_summary %>%
    mutate(
      plot_group = case_when(
        cohort == "Cohort 1" & session == "Session 2" ~ "Cohort 1 Post-Intervention", 
        cohort == "Cohort 2" & session == "Session 1" ~ "Cohort 2 Pre-Intervention",
        cohort == "Cohort 2" & session == "Session 2" ~ "Cohort 2 Post-Intervention"
      ),
      # Order: Cohort 1 Post, Cohort 2 Pre, Cohort 2 Post
      x_pos = case_when(
        cohort == "Cohort 1" & session == "Session 2" ~ 1,
        cohort == "Cohort 2" & session == "Session 1" ~ 2,
        cohort == "Cohort 2" & session == "Session 2" ~ 3
      ),
      # Simplified labels
      x_label = case_when(
        cohort == "Cohort 1" & session == "Session 2" ~ "Post-Intervention",
        cohort == "Cohort 2" & session == "Session 1" ~ "Pre-Intervention",
        cohort == "Cohort 2" & session == "Session 2" ~ "Post-Intervention"
      )
    )
  
  # Create a position dodge object with the user-specified width
  dodge_width <- bar_spacing
  pos_dodge <- position_dodge(width = dodge_width)
  
  # Create plot with the specified order and labels
  plot <- ggplot(df_summary, aes(x = factor(x_pos), y = mean_score, fill = interaction(cohort, condition_label))) +
    geom_bar(stat = "identity", position = pos_dodge, width = dodge_width * 0.75) +
    geom_errorbar(aes(ymin = mean_score - se, ymax = mean_score + se), 
                  position = pos_dodge, width = 0.2) +
    labs(x = "Session", 
         y = "Number of Bigrams Correctly Repeated") +
    scale_fill_manual(values = c(
      "Cohort 1.Foil" = "#d5bae6", 
      "Cohort 1.Target" = "#63278b",
      "Cohort 2.Foil" = "#ffc9db", 
      "Cohort 2.Target" = "#981340"
    ), guide = "none") + # Remove legend
    scale_y_continuous(limits = c(0, 0.5)) + # Set max y-axis to 0.6
    scale_x_discrete(labels = c(
      "1" = "Post-Intervention",  # Cohort 1
      "2" = "Pre-Intervention",   # Cohort 2
      "3" = "Post-Intervention"   # Cohort 2
    )) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 40, face = "bold", hjust = 0.5, margin = margin(b = 25)),
      axis.title.x = element_text(size = 32, face = "bold", margin = margin(t = 20)),
      axis.title.y = element_text(size = 32, face = "bold", margin = margin(r = 20)),
      axis.text = element_text(size = 28)
    )
  
  # Calculate max height for consistent star placement
  max_height <- 0.42
  
  # Get p-values
  p_value_c2_s1 <- results_list[["Cohort 2_Session 1"]]$bigram$p_value
  p_value_c2_s2 <- results_list[["Cohort 2_Session 2"]]$bigram$p_value
  p_value_c1_s2 <- results_list[["Cohort 1_Session 2"]]$bigram$p_value
  
  # Set labels based on p-values
  label_c2_s1 <- ifelse(p_value_c2_s1 < 0.001, "***", 
                        ifelse(p_value_c2_s1 < 0.01, "**", 
                               ifelse(p_value_c2_s1 < 0.05, "*", 
                                      ifelse(p_value_c2_s1 < 0.1, ".", ""))))
  
  label_c2_s2 <- ifelse(p_value_c2_s2 < 0.001, "***", 
                        ifelse(p_value_c2_s2 < 0.01, "**", 
                               ifelse(p_value_c2_s2 < 0.05, "*", 
                                      ifelse(p_value_c2_s2 < 0.1, ".", ""))))
  
  label_c1_s2 <- ifelse(p_value_c1_s2 < 0.001, "***", 
                        ifelse(p_value_c1_s2 < 0.01, "**", 
                               ifelse(p_value_c1_s2 < 0.05, "*", 
                                      ifelse(p_value_c1_s2 < 0.1, ".", ""))))
  
  # Add significance annotations
  if(label_c1_s2 != "") {
    plot <- plot + 
      annotate("text", x = 1, y = max_height, label = label_c1_s2, size = 15)
  }
  
  if(label_c2_s1 != "") {
    plot <- plot + 
      annotate("text", x = 2, y = max_height, label = label_c2_s1, size = 15)
  }
  
  if(label_c2_s2 != "") {
    plot <- plot + 
      annotate("text", x = 3, y = max_height, label = label_c2_s2, size = 15)
  }
  
  # Generate text summary of results
  result_summary <- paste0("Combined Cohorts SSL Bigram Analysis Results:\n\n")
  
  # Cohort 2, Session 1
  bigram_effect_c2_s1 <- results_list[["Cohort 2_Session 1"]]$bigram$effect
  bigram_p_value_c2_s1 <- results_list[["Cohort 2_Session 1"]]$bigram$p_value
  bigram_sig_c2_s1 <- ifelse(bigram_p_value_c2_s1 < 0.05, "significant", "not significant")
  
  # Cohort 2, Session 2
  bigram_effect_c2_s2 <- results_list[["Cohort 2_Session 2"]]$bigram$effect
  bigram_p_value_c2_s2 <- results_list[["Cohort 2_Session 2"]]$bigram$p_value
  bigram_sig_c2_s2 <- ifelse(bigram_p_value_c2_s2 < 0.05, "significant", "not significant")
  
  # Cohort 1, Session 2
  bigram_effect_c1_s2 <- results_list[["Cohort 1_Session 2"]]$bigram$effect
  bigram_p_value_c1_s2 <- results_list[["Cohort 1_Session 2"]]$bigram$p_value
  bigram_sig_c1_s2 <- ifelse(bigram_p_value_c1_s2 < 0.05, "significant", "not significant")
  
  result_summary <- paste0(
    result_summary,
    "Cohort 1, Post-Intervention:\n",
    "Bigrams: Target vs Foil difference is ", bigram_sig_c1_s2,
    " (Effect size = ", bigram_effect_c1_s2, ", p = ", bigram_p_value_c1_s2, ")\n\n",
    
    "Cohort 2, Pre-Intervention:\n",
    "Bigrams: Target vs Foil difference is ", bigram_sig_c2_s1,
    " (Effect size = ", bigram_effect_c2_s1, ", p = ", bigram_p_value_c2_s1, ")\n\n",
    
    "Cohort 2, Post-Intervention:\n",
    "Bigrams: Target vs Foil difference is ", bigram_sig_c2_s2,
    " (Effect size = ", bigram_effect_c2_s2, ", p = ", bigram_p_value_c2_s2, ")\n\n"
  )
  
  # Print the result summary to console
  print(result_summary)
  
  # Create the filename according to the specified convention
  file_name <- paste0("combined_cohorts_group_level_ssl_bigram.png")
  file_path <- file.path("~/wd_manuscript/plots/presentation_plots", file_name)
  
  # Save the plot
  ggsave(
    filename = file_path,
    plot = plot,
    width = 15,
    height = 8,
    dpi = 300
  )
  
  cat("Plot saved as:", file_name, "\n")
  
  # Return results
  return(list(
    plot = plot,
    summary_text = result_summary,
    models = results_list,
    data = long_df,
    file_name = file_name
  ))
}


# _______________________________________________________________________________________

# (3) pre-processing & creating dataframes
# _______________________________________________________________________________________

# generate answer keys for transcription (cohort 2 ssl only)
cohort2_ssl_answer_keys <- generate_ssl_answers(gorilla_ssl_lsl_output_cohort2)

# score & reformat ssl transcriptions
# prep raw transcriptions for scoring
cohort1_ssl_sicr_unscored_s2 <- ssl_sicr_transcript_cohort1_s2 %>%
  dplyr::select(id = Participant.Public.ID, trial_number = Trial.Number, target_tag = Condition, response = Response, key = Key) %>%
  mutate(target_tag = case_when(target_tag == 'target' ~ 1,
                                TRUE ~ 0))
cohort2_ssl_sicr_unscored_s1 <- ssl_unscored(ssl_sicr_transcript_cohort2_s1)
cohort2_ssl_sicr_unscored_s2 <- ssl_unscored(ssl_sicr_transcript_cohort2_s2)

# score ssl sicr transcriptions
cohort1_ssl_sicr_scored_s2 <- ssl_scored(cohort1_ssl_sicr_unscored_s2)
cohort2_ssl_sicr_scored_s1 <- ssl_scored(cohort2_ssl_sicr_unscored_s1)
cohort2_ssl_sicr_scored_s2 <- ssl_scored(cohort2_ssl_sicr_unscored_s2)

# generate long dataframes for lmer analyses
cohort1_ssl_sicr_long_mean_scored_s2 <- ssl_long_mean_scored(cohort1_ssl_sicr_scored_s2)
cohort2_ssl_sicr_long_mean_scored_s1 <- ssl_long_mean_scored(cohort2_ssl_sicr_scored_s1)
cohort2_ssl_sicr_long_mean_scored_s2 <- ssl_long_mean_scored(cohort2_ssl_sicr_scored_s2)

# generate wide dataframes for correlation analyses
cohort1_ssl_sicr_wide_mean_scored_s2 <- ssl_wide_mean_scored(cohort1_ssl_sicr_long_mean_scored_s2)
cohort2_ssl_sicr_wide_mean_scored_s1 <- ssl_wide_mean_scored(cohort2_ssl_sicr_long_mean_scored_s1)
cohort2_ssl_sicr_wide_mean_scored_s2 <- ssl_wide_mean_scored(cohort2_ssl_sicr_long_mean_scored_s2)

# generate dataframes for 2afc accuracy
cohort1_afc <- cohort1_afc_accuracy(gorilla_ssl_lsl_output_cohort1)
cohort2_afc <- cohort2_afc_accuracy(gorilla_ssl_lsl_output_cohort2)

# generate dataframes for assessments, demographics
cohort1_demographics <- wd_demographics(wd_demographics_cohort1)
cohort2_demographics <- wd_demographics(wd_demographics_cohort2)

cohort1_assessments <- wd_assessments(wd_assessments_cohort1)
cohort2_assessments <- wd_assessments(wd_assessments_cohort2)

View(cohort1_assessments)
View(cohort1_afc)

# generate combined dataframes (cohort 1 and cohort 2)
final_dataframe_cohort_1 <- cohort1_ssl_sicr_wide_mean_scored_s2 %>%
  full_join(cohort1_afc) %>%
  full_join(cohort1_assessments) %>%
  full_join(cohort1_demographics)

final_dataframe_cohort_2 <- cohort2_ssl_sicr_wide_mean_scored_s2 %>%
  full_join(cohort2_ssl_sicr_wide_mean_scored_s2) %>%
  full_join(cohort2_afc) %>%
  full_join(cohort2_assessments) %>%
  full_join(cohort2_demographics) %>%
  filter(!is.na(id))
 
 
# _______________________________________________________________________________________

# (4) methods: create participant tables
# _______________________________________________________________________________________
cohort1_participant_table <- create_participant_table(final_dataframe_cohort_1)
cohort2_participant_table <- create_participant_table(final_dataframe_cohort_2)


# _______________________________________________________________________________________

# (5) psychometric properties
# _______________________________________________________________________________________

# test-retest of ssl (cohort 2)
ssl_heatmap <- test_retest(final_dataframe_cohort_2, "ssl")
ssl_heatmap <- test_retest(final_dataframe_cohort_1, "ssl")

# internal reliability of ssl across cohorts
combined_internal_reliability(
  cohort2_ssl_sicr_scored_s1, 
  cohort2_ssl_sicr_scored_s2, 
  cohort2_afc_accuracy_by_trial,
  cohort1_ssl_sicr_scored_s2, 
  cohort1_afc_accuracy_by_trial
)

cohort2_afc_by_trial <- cohort2_afc_accuracy_by_trial %>%
  filter(`task` == 'ssl') %>%
  dplyr::select(-c(task, task_label))

cohort1_afc_by_trial <- cohort1_afc_accuracy_by_trial %>%
  filter(`task` == 'ssl') %>%
  dplyr::select(-c(task, task_label))


cohort2_ssl_sicr_scored_s1 <- cohort2_ssl_sicr_scored_s1 %>%
  mutate(
    syllable_count = case_when(
      response == "N/A" ~ 0,
      TRUE ~ str_count(response, "/") + 1
    )
  )

# Proportion of responses with at least 1, 2, or 3 syllables
prop_summary <- cohort2_ssl_sicr_scored_s1 %>%
  summarise(
    n = n(),
    prop_at_least_1 = mean(syllable_count >= 1, na.rm = TRUE),
    prop_at_least_2 = mean(syllable_count >= 2, na.rm = TRUE),
    prop_at_least_3 = mean(syllable_count >= 5, na.rm = TRUE)
  )

prop_summary

trigram_prop <- cohort2_ssl_sicr_scored_s2 %>%
  mutate(correct_trigrams = factor(correct_trigrams, levels = c(0, 1, 2))) %>%
  count(correct_trigrams, .drop = FALSE) %>%
  mutate(proportion = n / sum(n))

trigram_prop


partial_correlation_summary(final_dataframe_cohort_2, 'correct_serial_syllables.ssl_1_s1','correct_serial_syllables.ssl_1_s2','correct_serial_syllables.ssl_0_s1')
correlation_summary(final_dataframe_cohort_2, 'correct_serial_syllables.ssl_1_s1','correct_serial_syllables.ssl_1_s2')
correlation_summary(final_dataframe_cohort_2, 'correct_bigrams.ssl_1_s1','correct_bigrams.ssl_1_s2')
correlation_summary(final_dataframe_cohort_2, 'correct_trigrams.ssl_1_s1','correct_trigrams.ssl_1_s2')

correlation_summary(final_dataframe_cohort_2, 'correct_serial_syllables.ssl_0_s1','correct_serial_syllables.ssl_0_s2')
correlation_summary(final_dataframe_cohort_2, 'correct_bigrams.ssl_0_s1','correct_bigrams.ssl_0_s2')
correlation_summary(final_dataframe_cohort_2, 'correct_trigrams.ssl_0_s1','correct_trigrams.ssl_0_s2')

# ICC analysis
calculate_test_retest_icc <- function(data, t1, t2) {
  library(psych)
  
  icc_data <- na.omit(data.frame(t1 = data[[t1]], t2 = data[[t2]]))
  icc_results <- ICC(icc_data)
  
  print(icc_results)
}

calculate_test_retest_icc(final_dataframe_cohort_1, 'afc_accuracy_ssl_s1', 'afc_accuracy_ssl_s2')
calculate_test_retest_icc(final_dataframe_cohort_2, 'afc_accuracy_ssl_s1', 'afc_accuracy_ssl_s2')

calculate_test_retest_icc(final_dataframe_cohort_2, 'correct_serial_syllables.ssl_1_s1', 'correct_serial_syllables.ssl_1_s2')

calculate_test_retest_icc(final_dataframe_cohort_2, 'correct_bigrams.ssl_1_s1', 'correct_bigrams.ssl_1_s2')

calculate_test_retest_icc(final_dataframe_cohort_2, 'correct_trigrams.ssl_1_s1', 'correct_trigrams.ssl_1_s2')

# _______________________________________________________________________________________

# (6) group-level learning analyses
# _______________________________________________________________________________________

# tests whether 2afc accuracy is significantly above chance at the group level; summarizes
afc_accuracy_group_level <- function(df,
                                     cohort_label = "1",
                                     cols   = grep("^afc_accuracy_.*_s[12]$", names(df), value = TRUE),
                                     chance = 0.5,
                                     alpha  = 0.05) {
  
  if (!length(cols))
    stop("No columns matching 'afc_accuracy_*_s[12]' found.")
  
  # ------------ helper for one-sample test vs chance --------------------
  one_sample <- function(vec, colname) {
    
    normal <- shapiro.test(vec)$p.value > alpha
    if (normal) {
      res <- t.test(vec, mu = chance, alternative = "greater")
      stat_txt <- glue::glue("t({round(res$parameter,0)}) = {round(res$statistic,2)}")
      test_nm  <- "t-test"
    } else {
      res <- wilcox.test(vec, mu = chance, alternative = "greater")
      stat_txt <- glue::glue("V = {res$statistic}")
      test_nm  <- "Wilcoxon"
    }
    pval <- res$p.value
    d    <- (mean(vec) - chance) / sd(vec)
    
    sig <- if      (pval < .001) "significantly above chance (*** p < .001)"
    else if (pval < .01)  "significantly above chance (**  p < .01)"
    else if (pval < .05)  "significantly above chance (*  p < .05)"
    else if (pval < .10)  "marginally above chance (†  p < .10)"
    else                  "NOT significantly different from chance"
    
    glue::glue(
      "Cohort {cohort_label}, {colname}: {test_nm} → {sig}; ",
      "{stat_txt}, p = {scales::pvalue(pval)}, Cohen’s d = {round(d,2)}.")
  }
  
  # ------------ helper for paired improvement test ----------------------
  paired_test <- function(x1, x2, task_label) {
    
    diff <- x2 - x1
    diff <- diff[!is.na(diff)]
    
    if (length(diff) < 3)
      return(glue::glue(
        "Cohort {cohort_label}, {task_label} (S2-S1): could not compute – fewer than 3 paired observations."))
    
    normal <- shapiro.test(diff)$p.value > alpha
    if (normal) {
      res <- t.test(x2, x1, alternative = "greater", paired = TRUE)
      stat_txt <- glue::glue("t({round(res$parameter,0)}) = {round(res$statistic,2)}")
      test_nm  <- "paired t-test"
    } else {
      res <- wilcox.test(x2, x1, alternative = "greater", paired = TRUE)
      stat_txt <- glue::glue("V = {res$statistic}")
      test_nm  <- "Wilcoxon signed-rank"
    }
    pval <- res$p.value
    d    <- mean(diff) / sd(diff)   # paired Cohen’s d
    
    sig <- if      (pval < .001) "significant improvement (*** p < .001)"
    else if (pval < .01)  "significant improvement (**  p < .01)"
    else if (pval < .05)  "significant improvement (*  p < .05)"
    else if (pval < .10)  "marginal improvement (†  p < .10)"
    else                  "no significant improvement"
    
    glue::glue(
      "Cohort {cohort_label}, {task_label} (S2-S1): {test_nm} → {sig}; ",
      "{stat_txt}, p = {scales::pvalue(pval)}, Cohen’s d = {round(d,2)}.")
  }
  
  # ------------ 1 · one-sample chance tests ------------------------------
  for (col in cols) {
    vec <- as.numeric(df[[col]]); vec <- vec[!is.na(vec)]
    if (length(vec) < 3) {
      cat(glue::glue(
        "Cohort {cohort_label}, {col}: could not compute – fewer than 3 non-missing values.\n"),
        sep = "")
    } else {
      cat(one_sample(vec, col), "\n", sep = "")
    }
  }
  
  # ------------ 2 · paired improvement tests -----------------------------
  # find unique base names (everything before _s1 / _s2)
  base <- sub("_s[12]$", "", cols)
  for (b in unique(base)) {
    s1_col <- paste0(b, "_s1")
    s2_col <- paste0(b, "_s2")
    if (all(c(s1_col, s2_col) %in% names(df))) {
      x1 <- as.numeric(df[[s1_col]])
      x2 <- as.numeric(df[[s2_col]])
      # keep rows where both present
      keep <- !is.na(x1) & !is.na(x2)
      x1 <- x1[keep]; x2 <- x2[keep]
      
      task_label <- sub("^afc_accuracy_", "", b)  # e.g., lsl or ssl
      cat(paired_test(x1, x2, task_label), "\n", sep = "")
    }
  }
}

afc_accuracy_group_level(final_dataframe_cohort_1, cohort_label = "1")
afc_accuracy_group_level(final_dataframe_cohort_2, cohort_label = "2")

# plots 2afc group-level accuracy for manuscript
plot_afc_accuracy <- function(df1, df2, 
                              color1_s1, color1_s2, color2_s1, color2_s2,
                              cohort1_label = "Cohort 1", 
                              cohort2_label = "Cohort 2",
                              s1_label = "Pre",
                              s2_label = "Post",
                              y_label = "2AFC Accuracy") {
  library(ggplot2)
  
  # Calculate means and SEMs
  means <- c(
    mean(df1$afc_accuracy_ssl_s1, na.rm = TRUE),
    mean(df1$afc_accuracy_ssl_s2, na.rm = TRUE),
    mean(df2$afc_accuracy_ssl_s1, na.rm = TRUE),
    mean(df2$afc_accuracy_ssl_s2, na.rm = TRUE)
  )
  
  sems <- c(
    sd(df1$afc_accuracy_ssl_s1, na.rm = TRUE) / sqrt(sum(!is.na(df1$afc_accuracy_ssl_s1))),
    sd(df1$afc_accuracy_ssl_s2, na.rm = TRUE) / sqrt(sum(!is.na(df1$afc_accuracy_ssl_s2))),
    sd(df2$afc_accuracy_ssl_s1, na.rm = TRUE) / sqrt(sum(!is.na(df2$afc_accuracy_ssl_s1))),
    sd(df2$afc_accuracy_ssl_s2, na.rm = TRUE) / sqrt(sum(!is.na(df2$afc_accuracy_ssl_s2)))
  )
  
  # One-tailed t-tests against 0.5
  pvals <- c(
    t.test(df1$afc_accuracy_ssl_s1, mu = 0.5, alternative = "greater")$p.value,
    t.test(df1$afc_accuracy_ssl_s2, mu = 0.5, alternative = "greater")$p.value,
    t.test(df2$afc_accuracy_ssl_s1, mu = 0.5, alternative = "greater")$p.value,
    t.test(df2$afc_accuracy_ssl_s2, mu = 0.5, alternative = "greater")$p.value
  )
  
  # Significance stars
  get_stars <- function(p) {
    ifelse(p < 0.001, "***",
           ifelse(p < 0.01, "**",
                  ifelse(p < 0.05, "*", "")))
  }
  
  # Create data frame for plotting
  plot_data <- data.frame(
    cohort = factor(c(cohort1_label, cohort1_label, cohort2_label, cohort2_label), 
                    levels = c(cohort1_label, cohort2_label)),
    session = rep(c(s1_label, s2_label), 2),
    mean = means,
    sem = sems,
    stars = get_stars(pvals),
    group = factor(c(
      paste(cohort1_label, s1_label),
      paste(cohort1_label, s2_label),
      paste(cohort2_label, s1_label),
      paste(cohort2_label, s2_label)
    ), levels = c(
      paste(cohort1_label, s1_label),
      paste(cohort1_label, s2_label),
      paste(cohort2_label, s1_label),
      paste(cohort2_label, s2_label)
    )),
    color = c(color1_s1, color1_s2, color2_s1, color2_s2)
  )
  
  # Create plot
  p <- ggplot(plot_data, aes(x = cohort, y = mean, fill = group)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
    geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                  position = position_dodge(width = 0.9), width = 0.2, linewidth = 0.5) +
    geom_text(aes(label = stars, y = mean + sem + 0.02),
              position = position_dodge(width = 0.9), size = 15, vjust = 0) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray40", alpha = 0.3) +
    scale_fill_manual(
      values = c(color1_s1, color1_s2, color2_s1, color2_s2),
      name = NULL,
      labels = c(
        paste(cohort1_label, s1_label),
        paste(cohort1_label, s2_label),
        paste(cohort2_label, s1_label),
        paste(cohort2_label, s2_label)
      )
    ) +
    scale_y_continuous(limits = c(0, 0.85), expand = c(0, 0)) +
    labs(y = y_label, x = NULL) +
    theme_classic() +
    theme(
      axis.text = element_text(size = 20, color = "black"),
      axis.title = element_text(size = 20, face='bold'),
      axis.line = element_line(size = 0.5),
      legend.position = "right",
      legend.text = element_text(size = 20)
    )
  
  return(p)
}

plot_afc_accuracy(final_dataframe_cohort_1, final_dataframe_cohort_2, "#ebd6ff", "#7c51bd", "#ffc7d9", "#E91E63")

# tests whether targets are distinguished from foils on SICR; summarizes
wd_lmer_test_summary <- function(df, score_suffix = NULL, df_name = NULL, alpha = 0.05) {
  
  suppressPackageStartupMessages({
    library(dplyr); library(tidyr); library(lme4); library(lmerTest); library(stringr); library(glue)
  })
  
  # ── figure out the dataframe name (for nice messages) --------------------
  if (is.null(df_name)) {
    df_name_expr <- substitute(df)
    df_name <- if (is.name(df_name_expr)) as.character(df_name_expr) else "lmer_results"
  }
  
  # ── detect task type from name -------------------------------------------
  task_type <- if (grepl("lsl", df_name, ignore.case = TRUE)) "lsl"
  else if (grepl("ssl", df_name, ignore.case = TRUE)) "ssl"
  else NULL
  
  # ── auto-detect column suffix + prefixes (identical to original logic) ----
  if (is.null(score_suffix)) {
    poss_pref <- if (task_type == "lsl")
      c("correct_serial_letters", "correct_serial_syllables")
    else
      c("correct_serial_syllables", "correct_serial_letters")
    
    found <- FALSE
    for (pref in poss_pref) {
      for (suf in c(".lsl", ".ssl", "")) {
        if (all(c(paste0(pref, suf), paste0("correct_bigrams", suf)) %in% names(df))) {
          serial_col <- paste0(pref, suf)
          bigram_col <- paste0("correct_bigrams", suf)
          score_suffix <- suf
          found <- TRUE
          break
        }
      }
      if (found) break
    }
    if (!found) stop("Could not find matching serial + bigram columns automatically.")
  } else {
    serial_col <- if (task_type == "lsl" &&
                      paste0("correct_serial_letters", score_suffix) %in% names(df))
      paste0("correct_serial_letters", score_suffix)
    else if (paste0("correct_serial_syllables", score_suffix) %in% names(df))
      paste0("correct_serial_syllables", score_suffix)
    else
      stop("Specified serial column not found.")
    bigram_col <- paste0("correct_bigrams", score_suffix)
    if (!bigram_col %in% names(df))
      stop("Specified bigram column not found.")
  }
  
  serial_disp <- if (grepl("letters", serial_col)) "Serial Letters" else "Serial Syllables"
  
  # ── long format for a single call to lmer each ---------------------------
  long_df <- df %>%
    dplyr::select(id, target_tag,
                  serial = !!sym(serial_col),
                  bigram = !!sym(bigram_col)) %>%
    pivot_longer(cols = c(serial, bigram),
                 names_to = "score_type",
                 values_to = "score")
  
  # convert target_tag (0 / 1 foil/target) into factor
  long_df <- long_df %>%
    mutate(condition = factor(target_tag, levels = c(0,1),
                              labels = c("Foil","Target")))
  
  # ── run LMER for serial & bigram separately ------------------------------
  serial_model <- lmer(score ~ condition + (1|id),
                       data = filter(long_df, score_type == "serial"))
  bigram_model <- lmer(score ~ condition + (1|id),
                       data = filter(long_df, score_type == "bigram"))
  
  serial_p <- summary(serial_model)$coefficients["conditionTarget","Pr(>|t|)"]
  bigram_p <- summary(bigram_model)$coefficients["conditionTarget","Pr(>|t|)"]
  
  serial_est <- summary(serial_model)$coefficients["conditionTarget","Estimate"]
  bigram_est <- summary(bigram_model)$coefficients["conditionTarget","Estimate"]
  
  # ── helper to turn p into words ------------------------------------------
  sig_word <- function(p) {
    if (p < .001) "significant (*** p < .001)"
    else if (p < .01) "significant (** p < .01)"
    else if (p < .05) "significant (* p < .05)"
    else if (p < .10) "marginal († p < .10)"
    else "not significant"
  }
  
  # ── build summary string --------------------------------------------------
  cohort_txt  <- str_extract(df_name, "cohort[0-9]+")   %||% ""
  session_txt <- str_extract(df_name, "s[0-9]+")        %||% ""
  task_txt    <- task_type                              %||% ""
  
  header <- glue("{cohort_txt} {task_txt} {session_txt}") %>% str_trim()
  
  summary_lines <- c(
    glue("{serial_disp}: Target vs Foil difference is {sig_word(serial_p)} ",
         "(β = {round(serial_est, 3)}, p = {scales::pvalue(serial_p)})"),
    glue("Bigrams: Target vs Foil difference is {sig_word(bigram_p)} ",
         "(β = {round(bigram_est, 3)}, p = {scales::pvalue(bigram_p)})")
  )
  
  cat(header, "\n", 
      paste0("- ", summary_lines, collapse = "\n"), "\n", sep = "")
  
  invisible(list(
    serial_model = serial_model,
    bigram_model = bigram_model,
    summary_text = paste(summary_lines, collapse = " / ")
  ))
}

wd_lmer_test_summary(cohort1_ssl_sicr_long_mean_scored_s2)
wd_lmer_test_summary(cohort2_ssl_sicr_long_mean_scored_s1)
wd_lmer_test_summary(cohort2_ssl_sicr_long_mean_scored_s2)

# plots group-level SICR target vs foil for manuscript
plot_sicr_scores <- function(df1_s2, df2_s1, df2_s2,
                             color1_dark, color2_light, color2_dark,
                             cohort1_label = "Cohort 1",
                             cohort2_label = "Cohort 2",
                             s1_label = "Pre-intervention",
                             s2_label = "Post-intervention",
                             score_suffix = NULL,
                             y_label = "Number of Bigrams Correctly Repeated") {
  
  library(ggplot2)
  library(dplyr)
  library(lme4)
  library(lmerTest)
  library(tidyr)
  
  # Helper function to detect columns, calculate means, and run lmer
  get_data_and_pval <- function(df, score_suffix) {
    # Auto-detect column names
    if (is.null(score_suffix)) {
      for (suf in c(".ssl", ".lsl", "")) {
        serial_col <- paste0("correct_bigrams", suf)
        if (serial_col %in% names(df)) {
          score_suffix <- suf
          break
        }
      }
    }
    
    serial_col <- paste0("correct_bigrams", score_suffix)
    
    # Calculate means and SEM
    df_summary <- df %>%
      mutate(condition = factor(target_tag, levels = c(0, 1),
                                labels = c("Foil", "Target"))) %>%
      group_by(condition) %>%
      summarise(
        mean = mean(!!sym(serial_col), na.rm = TRUE),
        sem = sd(!!sym(serial_col), na.rm = TRUE) / sqrt(sum(!is.na(!!sym(serial_col)))),
        .groups = "drop"
      )
    
    # Run lmer
    lmer_df <- df %>%
      mutate(condition = factor(target_tag, levels = c(0, 1),
                                labels = c("Foil", "Target")))
    
    model <- lmer(as.formula(paste(serial_col, "~ condition + (1|id)")), data = lmer_df)
    p_val <- summary(model)$coefficients["conditionTarget", "Pr(>|t|)"]
    
    return(list(summary = df_summary, p_value = p_val))
  }
  
  # Get data and p-values for all dataframes
  c1_s2_data <- get_data_and_pval(df1_s2, score_suffix)
  c2_s1_data <- get_data_and_pval(df2_s1, score_suffix)
  c2_s2_data <- get_data_and_pval(df2_s2, score_suffix)
  
  # Add cohort and session info
  c1_s2 <- c1_s2_data$summary %>% mutate(cohort = cohort1_label, session = s2_label, p_val = c1_s2_data$p_value)
  c2_s1 <- c2_s1_data$summary %>% mutate(cohort = cohort2_label, session = s1_label, p_val = c2_s1_data$p_value)
  c2_s2 <- c2_s2_data$summary %>% mutate(cohort = cohort2_label, session = s2_label, p_val = c2_s2_data$p_value)
  
  # Significance stars function
  get_stars <- function(p) {
    ifelse(p < 0.001, "***",
           ifelse(p < 0.01, "**",
                  ifelse(p < 0.05, "*", "")))
  }
  
  # Combine all data
  plot_data <- bind_rows(c1_s2, c2_s1, c2_s2) %>%
    mutate(
      cohort = factor(cohort, levels = c(cohort1_label, cohort2_label)),
      session = factor(session, levels = c(s1_label, s2_label)),
      group = factor(paste(cohort, session), 
                     levels = c(
                       paste(cohort1_label, s2_label),
                       paste(cohort2_label, s1_label),
                       paste(cohort2_label, s2_label)
                     )),
      color = case_when(
        cohort == cohort1_label & session == s2_label ~ color1_dark,
        cohort == cohort2_label & session == s1_label ~ color2_light,
        cohort == cohort2_label & session == s2_label ~ color2_dark
      ),
      cohort_session = factor(paste(cohort, session, sep = "\n"),
                              levels = c(
                                paste(cohort1_label, s2_label, sep = "\n"),
                                paste(cohort2_label, s1_label, sep = "\n"),
                                paste(cohort2_label, s2_label, sep = "\n")
                              )),
      stars = get_stars(p_val)
    )
  
  # Calculate y position for stars (above the higher bar + sem)
  plot_data <- plot_data %>%
    group_by(cohort, session) %>%
    mutate(
      y_max = max(mean + sem),
      star_y = y_max + 0.02 * diff(range(plot_data$mean))
    ) %>%
    ungroup()
  
  # Create plot with facets
  p <- ggplot(plot_data, aes(x = condition, y = mean, fill = group)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
    geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                  position = position_dodge(width = 0.9), width = 0.2, linewidth = 0.5) +
    # Add significance stars between bars
    geom_text(data = plot_data %>% filter(condition == "Target"),
              aes(x = 1.5, y = star_y, label = stars),
              size = 15, vjust = 0) +
    facet_wrap(~ cohort_session, nrow = 1) +
    scale_fill_manual(
      values = c(color1_dark, color2_light, color2_dark),
      name = NULL,
      labels = c(
        paste(cohort1_label, s2_label),
        paste(cohort2_label, s1_label),
        paste(cohort2_label, s2_label)
      )
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    coord_cartesian(clip = "off") +
    labs(y = y_label, x = NULL) +
    theme_classic() +
    theme(
      axis.text = element_text(size = 20, color = "black"),
      axis.title = element_text(size = 20, face='bold'),
      axis.line = element_line(size = 0.5),
      strip.background = element_blank(),
      strip.text = element_blank(),
      panel.spacing = unit(0.3, "lines"),
      plot.margin = margin(t = 20, r = 10, b = 10, l = 10),
      legend.position = "right",
      legend.text = element_text(size = 20)
    )
  
  return(p)
}

plot_sicr_scores(
  cohort1_ssl_sicr_long_mean_scored_s2,
  cohort2_ssl_sicr_long_mean_scored_s1,
  cohort2_ssl_sicr_long_mean_scored_s2,
  color1_dark = "#7c51bd",
  color2_light = "#ffc7d9",
  color2_dark = "#E91E63",
  cohort1_label = "Cohort 1",
  cohort2_label = "Cohort 2"
)

# computes paired t-test for whether assessment scores improved pre- vs post- intervention
compare_scores <- function(df, var1, var2) {
  # var1 = pre, var2 = post
  scores_pre <- df[[var1]]
  scores_post <- df[[var2]]
  
  complete_cases <- complete.cases(scores_pre, scores_post)
  scores_pre_clean <- scores_pre[complete_cases]
  scores_post_clean <- scores_post[complete_cases]
  
  # One-tailed: post > pre
  test_result <- t.test(scores_post_clean, scores_pre_clean, 
                        paired = TRUE, 
                        alternative = "greater")
  
  mean_pre <- mean(scores_pre_clean, na.rm = TRUE)
  mean_post <- mean(scores_post_clean, na.rm = TRUE)
  sd_pre <- sd(scores_pre_clean, na.rm = TRUE)
  sd_post <- sd(scores_post_clean, na.rm = TRUE)
  n <- length(scores_pre_clean)
  mean_diff <- mean(scores_post_clean - scores_pre_clean, na.rm = TRUE)
  
  sd_diff <- sd(scores_post_clean - scores_pre_clean, na.rm = TRUE)
  cohens_d <- mean_diff / sd_diff
  
  cat("\n=== One-Tailed Paired t-test Results ===\n")
  cat("Alternative Hypothesis: var2 > var1\n\n")
  cat(sprintf("Variable 1 (%s):\n", var1))
  cat(sprintf("  Mean = %.2f (SD = %.2f)\n\n", mean_pre, sd_pre))
  cat(sprintf("Variable 2 (%s):\n", var2))
  cat(sprintf("  Mean = %.2f (SD = %.2f)\n\n", mean_post, sd_post))
  cat(sprintf("Mean Difference (var2 - var1): %.2f\n", mean_diff))
  cat(sprintf("Sample size (complete pairs): %d\n\n", n))
  cat(sprintf("t(%d) = %.3f, p (one-tailed) = %.4f\n", 
              test_result$parameter, 
              test_result$statistic, 
              test_result$p.value))
  cat(sprintf("95%% CI for mean difference: [%.2f, Inf]\n", 
              test_result$conf.int[1]))
  cat(sprintf("Cohen's d = %.3f\n\n", cohens_d))
  
  invisible(list(
    test = test_result,
    descriptives = data.frame(
      variable = c(var1, var2),
      mean = c(mean_pre, mean_post),
      sd = c(sd_pre, sd_post),
      n = c(n, n)
    ),
    effect_size = cohens_d
  ))
}

compare_scores(df=final_dataframe_cohort_1, 'taps_deletion_ss_pre', 'taps_deletion_ss_post')
compare_scores(df=final_dataframe_cohort_2, 'taps_deletion_ss_pre', 'taps_deletion_ss_post')

compare_scores(df=final_dataframe_cohort_1, 'dibels_nwf_cls_pre', 'dibels_nwf_cls_post')
compare_scores(df=final_dataframe_cohort_2, 'dibels_nwf_cls_pre', 'dibels_nwf_cls_post')

compare_scores(df=final_dataframe_cohort_1, 'dibels_nwf_wrc_pre', 'dibels_nwf_wrc_post')
compare_scores(df=final_dataframe_cohort_2, 'dibels_nwf_wrc_pre', 'dibels_nwf_wrc_post')

compare_scores(df=final_dataframe_cohort_1, 'dibels_orf_pre', 'dibels_orf_post')
compare_scores(df=final_dataframe_cohort_2, 'dibels_orf_pre', 'dibels_orf_post')

compare_scores(df=final_dataframe_cohort_1, 'towre_swe_ss_pre', 'towre_swe_ss_post')
compare_scores(df=final_dataframe_cohort_2, 'towre_swe_ss_pre', 'towre_swe_ss_post')

compare_scores(df=final_dataframe_cohort_1, 'towre_pde_ss_pre', 'towre_pde_ss_post')
compare_scores(df=final_dataframe_cohort_2, 'towre_pde_ss_pre', 'towre_pde_ss_post')

all_cohort_assessments <- read.csv('wd_all_cohorts_assessments.csv')

compare_scores(df=all_cohort_assessments, 'taps_deletion_ss_pre', 'taps_deletion_ss_post')
compare_scores(df=all_cohort_assessments, 'dibels_nwf_cls_pre', 'dibels_nwf_cls_post')
compare_scores(df=all_cohort_assessments, 'dibels_nwf_wrc_pre', 'dibels_nwf_wrc_post')
compare_scores(df=all_cohort_assessments, 'dibels_orf_pre', 'dibels_orf_post')
compare_scores(df=all_cohort_assessments, 'towre_swe_ss_pre', 'towre_swe_ss_post')
compare_scores(df=all_cohort_assessments, 'towre_pde_ss_pre', 'towre_pde_ss_post')

compare_scores <- function(df, pre_col, post_col) {
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  
  plot_df <- df %>%
    dplyr::select(pre = all_of(pre_col), post = all_of(post_col)) %>%
    pivot_longer(everything(), names_to = "time", values_to = "score") %>%
    mutate(time = factor(time, levels = c("pre", "post"), labels = c("Pre", "Post")))
  
  ggplot(plot_df, aes(x = time, y = score)) +
    geom_bar(stat = "summary", fun = "mean", fill = "#D3D3D3", width = 0.6) +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15) +
    theme_minimal() +
    theme(
      axis.line = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.ticks = element_blank(),
      text = element_text(size = 12)
    ) +
    labs(x = NULL, y = "Standard Score", title = "Pre vs. Post")
}

compare_scores(df=all_cohort_assessments, 'taps_deletion_ss_pre', 'taps_deletion_ss_post')
compare_scores(df=all_cohort_assessments, 'dibels_nwf_cls_pre', 'dibels_nwf_cls_post')
compare_scores(df=all_cohort_assessments, 'dibels_nwf_wrc_pre', 'dibels_nwf_wrc_post')
compare_scores(df=all_cohort_assessments, 'dibels_orf_pre', 'dibels_orf_post')
compare_scores(df=all_cohort_assessments, 'towre_swe_ss_pre', 'towre_swe_ss_post')
compare_scores(df=all_cohort_assessments, 'towre_pde_ss_pre', 'towre_pde_ss_post')

plot_change_histogram <- function(df, pre_col, post_col, label = NULL) {
  library(ggplot2)
  
  if (is.null(label)) label <- gsub("_ss_pre$|_pre$", "", pre_col)
  
  df$change <- df[[post_col]] - df[[pre_col]]
  
  ggplot(df, aes(x = change)) +
    geom_histogram(fill = "#D3D3D3", color = "white", binwidth = 1) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    theme_minimal() +
    theme(
      axis.line = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.ticks = element_blank(),
      text = element_text(size = 12)
    ) +
    labs(x = "Post \u2013 Pre Change", y = "Count", title = paste0(label, ": Post \u2013 Pre"))
}

plot_change_histogram(df=all_cohort_assessments, 'taps_deletion_ss_pre', 'taps_deletion_ss_post')
plot_change_histogram(df=all_cohort_assessments, 'dibels_nwf_cls_pre', 'dibels_nwf_cls_post')
plot_change_histogram(df=all_cohort_assessments, 'dibels_nwf_wrc_pre', 'dibels_nwf_wrc_post')
plot_change_histogram(df=all_cohort_assessments, 'dibels_orf_pre', 'dibels_orf_post')
plot_change_histogram(df=all_cohort_assessments, 'towre_swe_ss_pre', 'towre_swe_ss_post')
plot_change_histogram(df=all_cohort_assessments, 'towre_pde_ss_pre', 'towre_pde_ss_post')

# _______________________________________________________________________________________

# (7) correlation analyses
# _______________________________________________________________________________________

correlation_summary <- function(df, independent, dependent, alpha = 0.05) {
  
  df_name    <- deparse(substitute(df))
  cohort_tag <- str_extract(df_name, "cohort_?[0-9]+") %||%
    str_extract(df_name, "cohort[0-9]+")    %||% ""
  
  df[[independent]] <- as.numeric(df[[independent]])
  df[[dependent]]   <- as.numeric(df[[dependent]])
  
  sub <- df %>% dplyr::select(all_of(c(independent, dependent))) %>% drop_na()
  
  if (nrow(sub) < 3) {
    cat(cohort_tag, ": not enough data for", independent, "vs", dependent, "\n")
    return(invisible(NULL))
  }
  
  res  <- cor.test(sub[[independent]], sub[[dependent]], method = "pearson")
  r    <- round(res$estimate, 3)
  pval <- res$p.value
  n    <- nrow(sub)
  
  interp <- if      (pval < .001) "significant (*** p < .001)"
  else if (pval < .01)  "significant (** p < .01)"
  else if (pval < .05)  "significant (* p < .05)"
  else if (pval < .10)  "marginal († p < .10)"
  else                  "ns"
  
  cat(glue("{cohort_tag}: ρ({independent},{dependent}) = {r}, ",
           "p = {scales::pvalue(pval)}, n = {n} → {interp}\n"))
  
  # -------- only plot if p < .10 ------------------------------------------
  if (pval < 0.10) {
    ind_lab <- str_replace_all(independent, "_", " ")
    dep_lab <- str_replace_all(dependent,   "_", " ")
    
    p <- ggplot(sub, aes(.data[[independent]], .data[[dependent]])) +
      geom_point(color = "deeppink", size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", color = "lightpink", se = FALSE) +
      labs(
        x = ind_lab,
        y = dep_lab,
        title    = paste(cohort_tag, ":", ind_lab, "vs.", dep_lab),
        subtitle = paste("R =", r, "| p =", signif(pval, 3))
      ) +
      theme_minimal(base_family = "Times New Roman")
    
    print(p)   # send plot to the plotting device
  }
  
  invisible(list(r = r, p = pval, n = n, test = res))
}
partial_correlation_summary <- function(df,
                                        independent,
                                        dependent,
                                        control_vars,
                                        alpha = 0.05) {
  
  if (!requireNamespace("ppcor", quietly = TRUE))
    stop("Install the ‘ppcor’ package for partial correlations.")
  
  df_name    <- deparse(substitute(df))
  cohort_tag <- str_extract(df_name, "cohort_?[0-9]+") %||%
    str_extract(df_name, "cohort[0-9]+")    %||% ""
  
  all_vars <- c(independent, dependent, control_vars)
  df[all_vars] <- lapply(df[all_vars], as.numeric)
  
  sub <- df %>% dplyr::select(all_of(all_vars)) %>% drop_na()
  
  if (nrow(sub) < 4) {
    cat(cohort_tag, ": not enough data for partial ρ(", independent, ",", dependent, ")\n")
    return(invisible(NULL))
  }
  
  pc <- if (length(control_vars) == 1) {
    ppcor::pcor.test(sub[[independent]], sub[[dependent]],
                     sub[[control_vars]], method = "pearson")
  } else {
    ppcor::pcor.test(sub[[independent]], sub[[dependent]],
                     as.matrix(sub[control_vars]), method = "pearson")
  }
  
  r    <- round(pc$estimate, 3)
  pval <- pc$p.value
  n    <- nrow(sub)
  
  interp <- if      (pval < .001) "significant (*** p < .001)"
  else if (pval < .01)  "significant (** p < .01)"
  else if (pval < .05)  "significant (* p < .05)"
  else if (pval < .10)  "marginal († p < .10)"
  else                  "ns"
  
  cat(glue("{cohort_tag}: ρ({independent},{dependent} | {paste(control_vars, collapse=',')}) = {r}, ",
           "p = {scales::pvalue(pval)}, n = {n} → {interp}\n"))
  
  if (pval < 0.10) {
    ind_lab <- str_replace_all(independent, "_", " ")
    dep_lab <- str_replace_all(dependent,   "_", " ")
    
    p <- ggplot(sub, aes(.data[[independent]], .data[[dependent]])) +
      geom_point(color = "deeppink", size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", color = "lightpink", se = FALSE) +
      labs(
        x = ind_lab,
        y = dep_lab,
        title   = paste(cohort_tag, ":", ind_lab, "vs.", dep_lab),
        subtitle= paste("Controlled for:", paste(control_vars, collapse = ", ")),
        caption = paste("Partial R =", r, "| p =", signif(pval, 3))
      ) +
      theme_minimal(base_family = "Times New Roman")
    
    print(p)
  }
  
  invisible(list(r = r, p = pval, n = n, test = pc))
}

# pre-intervention 2afc vs pre-intervention assessment scores
correlation_summary(final_dataframe_cohort_1, 'afc_accuracy_ssl_s1', 'taps_deletion_ss_pre')
correlation_summary(final_dataframe_cohort_1, 'afc_accuracy_ssl_s1', 'dibels_nwf_cls_pre')
correlation_summary(final_dataframe_cohort_1, 'afc_accuracy_ssl_s1', 'dibels_nwf_wrc_pre')
correlation_summary(final_dataframe_cohort_1, 'afc_accuracy_ssl_s1', 'dibels_orf_pre')
correlation_summary(final_dataframe_cohort_1, 'afc_accuracy_ssl_s1', 'towre_swe_ss_pre')
correlation_summary(final_dataframe_cohort_1, 'afc_accuracy_ssl_s1', 'towre_pde_ss_pre')

correlation_summary(final_dataframe_cohort_2, 'afc_accuracy_ssl_s1', 'taps_deletion_ss_pre')
correlation_summary(final_dataframe_cohort_2, 'afc_accuracy_ssl_s1', 'dibels_nwf_cls_pre')
correlation_summary(final_dataframe_cohort_2, 'afc_accuracy_ssl_s1', 'dibels_nwf_wrc_pre')
correlation_summary(final_dataframe_cohort_2, 'afc_accuracy_ssl_s1', 'dibels_orf_pre')
correlation_summary(final_dataframe_cohort_2, 'afc_accuracy_ssl_s1', 'towre_swe_ss_pre')
correlation_summary(final_dataframe_cohort_2, 'afc_accuracy_ssl_s1', 'towre_pde_ss_pre')

correlation_summary(final_dataframe_cohort_1, 'afc_accuracy_ssl_s2', 'dibels_orf_post')
correlation_summary(final_dataframe_cohort_2, 'afc_accuracy_ssl_s2', 'dibels_orf_post')

# pre-intervention sicr vs pre-intervention assessment scores (cohort 2 only)
partial_correlation_summary(final_dataframe_cohort_2, 'correct_serial_syllables.ssl_1_s1','taps_deletion_ss_pre','correct_serial_syllables.ssl_0_s1')
partial_correlation_summary(final_dataframe_cohort_2, 'correct_bigrams.ssl_1_s1','taps_deletion_ss_pre','correct_bigrams.ssl_0_s1')

partial_correlation_summary(final_dataframe_cohort_2, 'correct_serial_syllables.ssl_1_s1','dibels_nwf_cls_pre','correct_serial_syllables.ssl_0_s1')
partial_correlation_summary(final_dataframe_cohort_2, 'correct_bigrams.ssl_1_s1','dibels_nwf_cls_pre','correct_bigrams.ssl_0_s1')

partial_correlation_summary(final_dataframe_cohort_2, 'correct_serial_syllables.ssl_1_s1','dibels_nwf_wrc_pre','correct_serial_syllables.ssl_0_s1')
partial_correlation_summary(final_dataframe_cohort_2, 'correct_bigrams.ssl_1_s1','dibels_nwf_wrc_pre','correct_bigrams.ssl_0_s1')

partial_correlation_summary(final_dataframe_cohort_2, 'correct_serial_syllables.ssl_1_s1','dibels_orf_pre','correct_serial_syllables.ssl_0_s1')
partial_correlation_summary(final_dataframe_cohort_2, 'correct_bigrams.ssl_1_s1','dibels_orf_pre','correct_bigrams.ssl_0_s1')

partial_correlation_summary(final_dataframe_cohort_2, 'correct_serial_syllables.ssl_1_s1','towre_swe_ss_pre','correct_serial_syllables.ssl_0_s1')
partial_correlation_summary(final_dataframe_cohort_2, 'correct_bigrams.ssl_1_s1','towre_swe_ss_pre','correct_bigrams.ssl_0_s1')

partial_correlation_summary(final_dataframe_cohort_2, 'correct_serial_syllables.ssl_1_s1','towre_pde_ss_pre','correct_serial_syllables.ssl_0_s1')
partial_correlation_summary(final_dataframe_cohort_2, 'correct_bigrams.ssl_1_s1','towre_pde_ss_pre','correct_bigrams.ssl_0_s1')

# FDR corrections / correcting for multiple comparisons / p-value adjustments

all_p_values <- c(
  # cohort 1 - 2afc accuracy
  # p_taps_2afc, p_dibels_nwf_cls_2afc, p_dibels_nwf_wrc_2afc, p_dibels_orf_2afc, p_towre_swe_2afc, p_towre_pde_2afc
  0.0282, 0.390, 0.365, 0.185, 0.205, 0.418,
  
  # cohort 2 - 2afc accuracy
  # p_taps_2afc, p_dibels_nwf_cls_2afc, p_dibels_nwf_wrc_2afc, p_dibels_orf_2afc, p_towre_swe_2afc, p_towre_pde_2afc
  0.139, 0.000618, 0.00746, 0.0159, 0.205, 0.249,
  
  # cohort 2 - sicr syllables
  # p_taps_syl, p_dibels_nwf_cls_syl, p_dibels_nwf_wrc_syl, p_dibels_orf_syl, p_towre_swe_syl, p_towre_pde_syl
  0.476, 0.0382, 0.132, 0.101, 0.117, 0.125,
  
  # Cohort 2 - sicr bigrams
  # p_taps_big, p_dibels_nwf_cls_big, p_dibels_nwf_wrc_big, p_dibels_orf_big, p_towre_swe_big, p_towre_pde_big
  0.104, 0.00656, 0.0878, 0.0567, 0.0339, 0.376
)

p.adjust(all_p_values, method = "BH")

# pre-intervention 2afc as a predictor of assessment gains
partial_correlation_summary(final_dataframe_cohort_1, 'afc_accuracy_ssl_s1','taps_deletion_ss_post','taps_deletion_ss_pre')
partial_correlation_summary(final_dataframe_cohort_1, 'afc_accuracy_ssl_s1','dibels_nwf_cls_post','dibels_nwf_cls_pre')
partial_correlation_summary(final_dataframe_cohort_1, 'afc_accuracy_ssl_s1','dibels_nwf_wrc_post','dibels_nwf_wrc_pre')
partial_correlation_summary(final_dataframe_cohort_1, 'afc_accuracy_ssl_s1','dibels_orf_post','dibels_nwf_wrc_pre')
partial_correlation_summary(final_dataframe_cohort_1, 'afc_accuracy_ssl_s1','towre_swe_ss_post','towre_swe_ss_pre')
partial_correlation_summary(final_dataframe_cohort_1, 'afc_accuracy_ssl_s1','towre_pde_ss_post','towre_pde_ss_pre')

partial_correlation_summary(final_dataframe_cohort_2, 'afc_accuracy_ssl_s1','taps_deletion_ss_post','taps_deletion_ss_pre')
partial_correlation_summary(final_dataframe_cohort_2, 'afc_accuracy_ssl_s1','dibels_nwf_cls_post','dibels_nwf_cls_pre')
partial_correlation_summary(final_dataframe_cohort_2, 'afc_accuracy_ssl_s1','dibels_nwf_wrc_post','dibels_nwf_wrc_pre')
partial_correlation_summary(final_dataframe_cohort_2, 'afc_accuracy_ssl_s1','dibels_orf_post','dibels_orf_pre')
partial_correlation_summary(final_dataframe_cohort_2, 'afc_accuracy_ssl_s1','towre_swe_ss_post','towre_swe_ss_pre')
partial_correlation_summary(final_dataframe_cohort_2, 'afc_accuracy_ssl_s1','towre_pde_ss_post','towre_pde_ss_pre')



# FDR corrections / correcting for multiple comparisons / p-value adjustments

all_p_values_longitudinal <- c(
  # cohort 1 - 2afc accuracy
  # p_taps_2afc, p_dibels_nwf_cls_2afc, p_dibels_nwf_wrc_2afc, p_dibels_orf_2afc, p_towre_swe_2afc, p_towre_pde_2afc
  0.00284, 0.0192, 0.0418, 0.194, 0.156, 0.0193,
  
  # cohort 2 - 2afc accuracy
  # p_taps_2afc, p_dibels_nwf_cls_2afc, p_dibels_nwf_wrc_2afc, p_dibels_orf_2afc, p_towre_swe_2afc, p_towre_pde_2afc
  0.0201, 0.104, 0.267, 0.0449, 0.463, 0.0584
)

p.adjust(all_p_values_longitudinal, method = "BH")

# produces visualizations of correlations
correlation_plot <- function(data, 
                             x_var, 
                             y_var,
                             control_var = NULL,
                             x_label = NULL,
                             y_label = NULL,
                             color = "#E91E63",
                             method = "pearson",
                             jitter = FALSE,
                             jitter_width = 0.1,
                             jitter_height = 0.1) {
  
  # Load required libraries
  require(ggplot2)
  require(dplyr)
  require(ppcor)  # For partial correlations
  
  # Create labels if not provided
  if (is.null(x_label)) x_label <- x_var
  if (is.null(y_label)) y_label <- y_var
  
  # Extract needed columns and remove NAs
  if (is.null(control_var)) {
    analysis_data <- data[, c(x_var, y_var)]
  } else {
    analysis_data <- data[, c(x_var, y_var, control_var)]
  }
  valid_data <- na.omit(analysis_data)
  
  # Check if we have enough data
  min_n <- if (is.null(control_var)) 3 else 4
  if (nrow(valid_data) < min_n) {
    stop(sprintf("Not enough valid data points for correlation (n < %d)", min_n))
  }
  
  # Calculate correlation (partial or regular)
  if (is.null(control_var)) {
    # Regular correlation
    cor_result <- cor.test(valid_data[[x_var]], 
                           valid_data[[y_var]], 
                           method = method)
    r_value <- cor_result$estimate
    p_value <- cor_result$p.value
  } else {
    # Partial correlation
    pcor_result <- pcor.test(valid_data[[x_var]], 
                             valid_data[[y_var]], 
                             valid_data[[control_var]], 
                             method = method)
    r_value <- pcor_result$estimate
    p_value <- pcor_result$p.value
  }
  
  # Create label with r and p
  label_text <- sprintf("r = %.3f\np = %.3f", r_value, p_value)
  
  # Calculate position for label (85% along x-axis, predicted y)
  x_range <- range(valid_data[[x_var]], na.rm = TRUE)
  x_pos <- x_range[1] + diff(x_range) * 0.85
  
  # Use vectors directly for lm and predict
  x_vec <- valid_data[[x_var]]
  y_vec <- valid_data[[y_var]]
  lm_fit <- lm(y_vec ~ x_vec)
  y_pos <- as.numeric(predict(lm_fit, newdata = data.frame(x_vec = x_pos)))
  
  # Prepare plot data with renamed columns for ggplot
  plot_data <- data.frame(
    x_value = valid_data[[x_var]],
    y_value = valid_data[[y_var]]
  )
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = x_value, y = y_value))
  
  # Add points with or without jitter
  if (jitter) {
    p <- p + geom_jitter(alpha = 0.6, size = 5, color = color,
                         width = jitter_width, height = jitter_height)
  } else {
    p <- p + geom_point(alpha = 0.6, size = 5, color = color)
  }
  
  # Add regression line
  p <- p + geom_smooth(method = "lm", se = FALSE, color = color, size = 1)
  
  # Add text label with r and p
  x_margin <- diff(x_range) * 0.15
  
  # Finalize plot
  p <- p +
    scale_x_continuous(breaks = seq(floor(x_range[1] / 5) * 5, 
                                    ceiling(x_range[2] / 5) * 5, 
                                    by = 0.1),
                       limits = c(x_range[1], x_range[2] + x_margin)) +
    labs(x = x_label, y = y_label) +
    theme_minimal() +
    theme(
      axis.title = element_text(face = "bold", size = 18),
      axis.text = element_text(size = 18),
      axis.line = element_line(color = "#4A4A4A", size = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.position = "none"
    )
  
  return(p)
}

correlation_plot(final_dataframe_cohort_2, 
                 x_var = 'correct_bigrams.ssl_1_s1', 
                 y_var = 'dibels_nwf_cls_pre',
                 control_var = 'correct_bigrams.ssl_0_s1',
                 x_label = 'Number of Bigrams Correctly Repeated\n(Pre-intervention)',
                 y_label = 'DIBELS Non-word Fluency Correct\nLetter Sounds (Pre-intervention)',
                 color = '#E91E63')

correlation_plot(final_dataframe_cohort_2, 
                 x_var = 'correct_bigrams.ssl_1_s1', 
                 y_var = 'towre_swe_ss_pre',
                 control_var = 'correct_bigrams.ssl_0_s1',
                 x_label = 'Number of Bigrams Correctly Repeated\n(Pre-intervention)',
                 y_label = 'TOWRE Sight Word Efficiency\nStandard Score (Pre-intervention)',
                 color = '#E91E63')

correlation_plot(final_dataframe_cohort_2, 
                 x_var = 'afc_accuracy_ssl_s1', 
                 y_var = 'dibels_nwf_cls_pre',
                 x_label = '2AFC Accuracy (Pre-Intervention)',
                 y_label = 'DIBELS Non-word Fluency Correct\nLetter Sounds (Pre-intervention)',
                 color = '#E91E63')

correlation_plot(final_dataframe_cohort_2, 
                 x_var = 'afc_accuracy_ssl_s1', 
                 y_var = 'taps_deletion_ss_post',
                 control_var = 'taps_deletion_ss_pre',
                 x_label = '2AFC Accuracy (Pre-Intervention)',
                 y_label = 'TAPS-4 Deletion Standard Score\n(Post-intervention)',
                 color = '#E91E63')

correlation_plot(final_dataframe_cohort_2, 
                 x_var = 'afc_accuracy_ssl_s1', 
                 y_var = 'dibels_orf_post',
                 control_var = 'dibels_orf_pre',
                 x_label = '2AFC Accuracy (Pre-Intervention)',
                 y_label = 'DIBELS Oral Reading Fluency Words Correct\n(Post-intervention)',
                 color = '#E91E63')

correlation_plot(final_dataframe_cohort_1, 
                 x_var = 'afc_accuracy_ssl_s1', 
                 y_var = 'taps_deletion_ss_post',
                 control_var = 'taps_deletion_ss_pre',
                 x_label = '2AFC Accuracy (Pre-Intervention)',
                 y_label = 'TAPS-4 Deletion Standard Score\n(Post-intervention)',
                 color = '#682ebf')
