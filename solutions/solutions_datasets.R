#' ---
#' author: 'Jan Vanhove'
#' title: 'Solutions Datasets'
#' date: '2025/03/06'
#' output: 
#'  html_document:
#'    toc: true
#'    toc_float: true
#'    number_sections: true
#'    theme: sandstone
#'    highlight: tango
#'    dev: svg
#'    df_print: paged
#' ---

#' # Many ways to skin a cat
#' These are just my solutions, but usually there are many
#' correct ways to solve these problems.
#' 
#' # Preliminaries
#' Load the packages required.
library(here)
library(tidyverse)
library(readxl)
#' 
#' # Exercise 1
vdb2018 <- read_xlsx(here("data", "VanDenBroek2018.xlsx"), sheet = 2)
vdb2018

#' # Exercise 2
#' Compare your answers with the result of running the commands
#' in the exercise.
#' 
#' # Exercise 3
#' We first need to recreate the datasets.
#' These commands are copied verbatim from the script.
translations <- read_csv(here("data", "uebersetzungen.csv"))
participants <- read_csv(here("data", "versuchspersonen.csv"))
items <- read_csv(here("data", "woerter.csv"))
all_data <- left_join(x = translations, y = participants)
all_data <- left_join(x = translations, y = participants, by = "Versuchsperson")
#' Next,
d1 <- all_data |> 
  filter(Übersetzung == "vorsichtig")
d1 # 4 rows
d2 <- all_data |> 
  filter(Übersetzung != "vorsichtig")
d2 # 12 rows
#' Alternatively, use `nrow()`:
nrow(d1)
nrow(d2)
#' However, $4 + 12 \neq 20$:
nrow(all_data)
#' The `filter()` commands selected those rows
#' for which `Übersetzung` is _known_ to be
#' `"vorsichtig"` or _known_ to not be `"vorsichtig"`,
#' respectively. However, `NA` values are neither
#' known to be `"vorsichtig"` or known not to be
#' `"vorsichtig"` and hence are excluded from both tibbles.
#' If we want to include them:
d3 <- all_data |> 
  filter(Übersetzung != "vorsichtig" | is.na(Übersetzung))
d3 # 16 rows

#' # Exercise 4
#' First, recreate the tibbles. Again,
#' these commands are copied verbatim from the script.
skills <- read_csv(here("data", "helascot_skills.csv"))
metrics <- read_csv(here("data", "helascot_metrics.csv"))
ratings <- read_csv(here("data", "helascot_ratings.csv"))
#' To only keep ratings from monolingual French speakers,
#' add the corresponding `filter()` command to the pipeline.
#' Note that only texts written in French are retained
#' as monolingual French raters only rated such texts.
rating_per_text <- ratings |> 
  filter(Rater_NativeLanguage == "mono-French") |> 
  group_by(Text, Subject, Text_Language, Text_Type, Time) |> 
  summarise(mean_rating = mean(Rating),
            n_ratings = n(),
            .groups = "drop")
rating_per_text

#' # Exercise 5
#' 1. Semicolons.
#' 2. Skip straight to 3.
#' 3. Like so
writing <- read_csv2(here("data", "writing_data.csv"),
                     locale = locale(encoding = "Windows-1252"))
#' 4. 473 rows and 288 columns.
writing

#' # Exercise 6
#' The first `select()` commands retains the 
#' columns `VPNDID` as well as those whose names 
#' start with `"Sco"` or `"P"`.
#' Some googling or a quick perusal of the
#' `tidyselect` documentation (see Section 6.3)
#' suggests we can use `where(is.numeric)`
#' to then only retain the numeric columns (as 
#' well as `VPNID`):
writing_long <- writing |> 
  select(VPNID, starts_with("Sco"), starts_with("P")) |> 
  select(VPNID, where(is.numeric))
writing_long # 473 rows, 250 cols
#' To make this tibble actually longer, we use `pivot_longer()`.
#' We want to put all the values occurring in the columns starting with `"Sco"`
#' or `"P"` in a single column, which we can achieve like so:
writing_long <- writing_long |>
  pivot_longer(cols = starts_with("Sco") | starts_with("P"),
               names_to = "criterion",
               values_to = "score")
#' Alternatively, we could have used
#' `cols = -VPNID` to select all columns _except_ for `VPNID`,
#' or `cols = Sco1R_Nb_mots:Sco3R_Destinataire` to select
#' all columns between these two (including).
#' The resulting tibble contains 117,777 columns, though only the first 10,000
#' are displayed by default:
writing_long
dim(writing_long) # rows, cols

#' # Exercise 7
#' Define the function `extract_info()` as in the script:
extract_info <- function(my_strings, i, my_regex = "([^\\d]+)(\\d)([^_]*)_(.*)$") {
  str_match(my_strings, my_regex)[, i]
}
#' Use `mutate()` to add the different pieces of information
#' to `writing_long`. Don't forget to drop the `criterion` (small c)
#' column as instructed -- it'll make life easier later.
writing_long <- writing_long |> 
  mutate(
    Language = extract_info(criterion, 2),
    Time = extract_info(criterion, 3),
    TextType = extract_info(criterion, 4),
    Criterion = extract_info(criterion, 5)
  ) |> 
  select(-criterion)
writing_long
#' Alternatively, you could've selected all other variables
#' rather than deselecting `criterion`.
#' 
#' # Exercise 8
#' Use `filter()`:
argumentative <- writing_long |> 
  filter(TextType == "A")
narrative <- writing_long |> 
  filter(TextType == "R")
argumentative # 59,125 rows
dim(argumentative)
narrative # 58,179 rows
dim(narrative)

#' # Exercise 9
#' 2. We need the first sheet of the Excel file:
criteria_arg <- read_excel(here("data", "scoring_criteria.xlsx"), sheet = 1)
#' 3. IMHO, the easiest option is to use `semi_join()`:
argumentative <- argumentative |> 
  semi_join(criteria_arg)
argumentative # 32,637 rows remaining
#' Alternatively, use `filter(Criterion %in% criteria_arg$Criterion)`.
#' 
#' 4. Like so:
argumentative <- argumentative |> 
  filter(!is.na(score))

#' 5. Using `pivot_wider()` and using the
#' fact that each text is a unique combination of `VPNID`, `Language` and `Time`:
argumentative <- argumentative |> 
  pivot_wider(names_from = Criterion,
              values_from = score)
argumentative # 2,164 rows, 16 columns

#' 6. **This is by far the most difficult part of the entire assignment series.**
#' 
#' As suggested, we use `summary()`.
#' Several columns contain missing data, and 
#' the problem is particularly pronounced for `Salutation`.
summary(argumentative)
#' We also see that the columns `Raisons`, `Object`, `Destinataire`, and
#' `Maj` have valued exceeding the corresponding `MaxScore` in `scoring_criteria.xslx`.
#' This is most easily verified by eye, but below is
#' one way to check this using R. To this end, we
#' make the tibble longer, join it with the criteria,
#' and check the rows where `score` exceeds `MaxScore`:
score_exceeds_max <- argumentative |> 
  pivot_longer(Lieu:Synt,
               names_to = "Criterion",
               values_to = "score") |> 
  left_join(criteria_arg) |> 
  filter(score > MaxScore)
score_exceeds_max
#' It's plausible that some of the raters didn't quite
#' follow the scoring grid. For instance, for the argumentative text,
#' points were awarded for each reason the child put forward,
#' but these points should have been capped at 3.
#' In one case, the rater probably just counted the number of reasons
#' put forward without capping the score.
#' A defensible solution to deal with such cases is to
#' do the capping manually. We'll do this later.
#' 
#' The missing values present a quandry of a different nature.
#' Since `Salutation` was missing most often, let's inspect
#' the original dataset, and extract the columns
#' containing `Salutation`:
writing |> 
  select(contains("Salutation"))
#' By default `contains()` ignores case,
#' which is a boon here. We immediately see
#' that the `P1A_salutation` and `P3A_salutation` 
#' column feature a lower-case `saluation`,
#' whereas `P2A_Psalutation` contains an additional letter.
#' 
#' To fix this error without editing the spreadsheet, we can use 
#' `mutate()` to create additional, correctly spelt, variables.
#' 
#' Let's delve into the other missing values. The commands below output
#' all texts for which some data is missing (disregarding the `Salutation` column),
#' 67 texts in total:
has_missing <- argumentative |> 
  select(-Salutation) |> 
  pivot_longer(Lieu:Synt,
               names_to = "Criterion",
               values_to = "score") |> 
  filter(is.na(score)) |> 
  select(VPNID, Language, Time, TextType) |> 
  distinct()
has_missing
#' If you inspect the tibble `has_missing` closely, you'll notice that 
#' participant `AI_CP_11`, who belongs to the Portuguese comparison group (hence `CP`), 
#' seems to have missing data for the language of eduction (`Sco`).
has_missing |> 
  filter(VPNID == "AI_CP_11")
#' Let's inspect their data in `argumentative`:
argumentative |> 
  filter(VPNID == "AI_CP_11")
#' We note that `AI_CP_11` has data for both `Sco` and `P`.
#' At each `Time`, though, the `Sco` and `P` match.
#' This leads us to suspect that the Portuguese comparison group,
#' for whom Portuguese was the language of instruction, had their data
#' entered _twice_: once in the `Sco` columns and once in the `P` columns.
#' We can verify this like so
argumentative |> 
  mutate(group = str_split_i(VPNID, "_", 2)) |> 
  filter(group == "CP") |> 
  filter(Language == "P")
argumentative |> 
  mutate(group = str_split_i(VPNID, "_", 2)) |> 
  filter(group == "CP") |> 
  filter(Language == "Sco")

#' With more intimate knowledge of the dataset, we'd have known this earlier.
#' These double entries are bound to lead to confusion, so let's get rid
#' of the `Sco` values for these participants:
argumentative <- argumentative |> 
  filter(!(str_detect(VPNID, "CP") & Language == "Sco"))

#' We recompute `has_missing` and `semi_join()` it with `writing_long`
#' to inspect all values associated with texts for which at least one value
#' is missing:
has_missing <- argumentative |> 
  select(-Salutation) |> 
  pivot_longer(Lieu:Synt,
               names_to = "Criterion",
               values_to = "score") |> 
  filter(is.na(score)) |> 
  select(VPNID, Language, Time, TextType) |> 
  distinct()

writing_long |> 
  semi_join(has_missing)
#' Combing through the resulting tibble manually, we detect a couple of patterns.
#' 
#' First, for several texts with some `NA` values, the only non-`NA` values
#' are for `Ponct`, `Maj`, and `Synt`, and these values are all 0. Perhaps these
#' values were preset to 0 in the spreadsheet and were only changed if needed?
#' And if so, perhaps they weren't then set to `NA` for texts that were never written?
#' 
#' **Tentative conclusion:** If the only non-`NA`-values are `Ponct`, `Maj`, and `Synt` 
#' and all of these are 0, then the text was never written. So these three 
#' 0 values should actually be `NA`.
#' 
#' Second, for several of the other texts, most of the data are available,
#' particularly the entry `Nb_mots` (text length), which indicates that these
#' texts were definitely written. It seems plausible that the other `NA` values
#' should actually be `0`!
#' 
#' **Tentative conclusion:** For texts for which `Nb_mots` is _not_ `NA`,
#' missing values should be converted to 0.
#' 
#' We can solve both of these problems by only retaining texts for which 
#' `Nb_mots` is not `NA`!
#' 
#' Now for the full proposed solution. In practice, you wouldn't _add_
#' these steps to your script; you'd change the script you had.
#' 
#' We read in the data again and correct the spelling errors in the variable names.
writing <- read_csv2(here("data", "writing_data.csv"),
                     locale = locale(encoding = "Windows-1252")) |> 
  mutate(
    P1A_Salutation = P1A_salutation,
    P3A_Salutation = P3A_salutation,
    P2A_Salutation = P2A_Psalutation
  ) |> 
  select(-P1A_salutation, -P2A_Psalutation, -P3A_salutation)
# Carry out same steps as previously
writing_long <- writing |> 
  select(VPNID, starts_with("Sco"), starts_with("P")) |> 
  select(VPNID, where(is.numeric))

writing_long <- writing_long |>
  pivot_longer(cols = starts_with("Sco") | starts_with("P"),
               names_to = "criterion",
               values_to = "score")

writing_long <- writing_long |> 
  mutate(
    Language = extract_info(criterion, 2),
    Time = extract_info(criterion, 3),
    TextType = extract_info(criterion, 4),
    Criterion = extract_info(criterion, 5)
  ) |> 
  select(-criterion)

argumentative <- writing_long |> 
  filter(TextType == "A")

narrative <- writing_long |> 
  filter(TextType == "R")

#' Recall that we need to get rid of texts where `Nb_mots` is `NA`
#' as well as texts written by members of the Portuguese comparison
#' group where `Language` reads `Sco`.
#' We can't just remove the rows where `Criterion` is `Nb_mots`
#' and where `score` is `NA` as this would leave in the entries
#' on the other criteria for the same texts.
#' So we need to convert the dataset to a wider format again,
#' drop the texts, and then convert them to a longer format again.
#' Like so:
argumentative <- argumentative |> 
  pivot_wider(names_from = "Criterion", values_from = "score") |> 
  filter(!is.na(Nb_mots)) |> 
  filter(!(str_detect(VPNID, "CP") & Language == "Sco")) |> 
  pivot_longer(Nb_mots:Verbal, names_to = "Criterion", values_to = "score")

#' Other missing values should be set to 0:
argumentative <- argumentative |> 
  mutate(score = case_when(
    is.na(score) ~ 0,
    .default     = score
  ))

#' We can fix the problem with the scores exceeding `MaxScore` at this stage.
#' Instead of using `semi_join()`, use `right_join()`: We want to 
#' retain just the data corresponding to one of the scored criteria.
#' Then clamp the `score` with `MaxScore`, e.g., using `pmin()`.
#' Then get rid of `MaxScore` and `Description` again.
#' While we're at it, also replace `NA` scores with `0`:
argumentative <- argumentative |> 
  right_join(criteria_arg) |> 
  mutate(score = pmin(score, MaxScore)) |> 
  select(-MaxScore, -Description)

#' We convert the tibble to a wider format again:
argumentative <- argumentative |> 
  pivot_wider(names_from = Criterion,
              values_from = score)

#' It seems that a new problem has been created. Some data are still missing,
#' and the `Organisateur` variable seems to be mostly missing!
summary(argumentative)

#' This is due to another spelling issue... It's probably easiest to change
#' the criterion name in the scoring criteria tibble and redo everything.
#' (You're getting the full immersive experience :))
writing |> select(contains("Organisateur"))

#' Let's try again:
criteria_arg <- criteria_arg |> 
  mutate(
    Criterion = case_when(
      Criterion == "Organisateur" ~ "Organisateurs",
      .default                    = Criterion
    )
  )

argumentative <- writing_long |> 
  filter(TextType == "A") |> 
  pivot_wider(names_from = "Criterion", values_from = "score") |> 
  filter(!is.na(Nb_mots)) |> 
  filter(!(str_detect(VPNID, "CP") & Language == "Sco")) |> 
  pivot_longer(Nb_mots:Verbal, names_to = "Criterion", values_to = "score") |> 
  right_join(criteria_arg) |> 
  mutate(score = pmin(score, MaxScore)) |> 
  select(-MaxScore, -Description) |> 
  mutate(score = case_when(
    is.na(score) ~ 0, 
    .default     = score)
  ) |> 
  pivot_wider(names_from = Criterion,
              values_from = score)
summary(argumentative)
#' That seems to have worked.
#' 
#' 7. Convert the tibble to a longer format:
argumentative_long <- argumentative |> 
  pivot_longer(Lieu:Synt, values_to = "score", names_to = "Criterion")
#' Then summarise by text:
text_scores <- argumentative_long |> 
  group_by(VPNID, Language, Time, TextType) |> 
  summarise(total_score_arg = sum(score),
            .groups = "drop")

#' 8.
text_scores |> 
  select(VPNID, Language, Time, total_score_arg) |> 
  write_csv(here("data", "arg_scores.csv"))

#' 
#' # Exercise 10
#' 1.
my_scores <- read_csv(here("data", "arg_scores.csv"))
#' 2.
old_scores <- read_csv(here("data", "helascot_skills.csv"))
#' 3. In `my_scores`, `Language` distinguishes between `P` and `Sco`.
#' In `old_scores`, `LanguageTested` distinguishes between `French`, `German` and `Portuguese`.
#' We can add a column `Language` to `old_scores` that contains `P` and `Sco` as required:
old_scores <- old_scores |> 
  mutate(Language = case_when(
    LanguageTested == "Portuguese" ~ "P",
    .default                       = "Sco"
  )) |> 
  select(-LanguageTested)
#' Additionally, `my_scores` contains the variable `VPNID`; `old_scores` contains the same
#' information in the column `Subject`.
#' We use `full_join()` to stitch together both tibbles without losing any entries,
#' but we need to specify that `Subject` and `VPNID` are to be matched:
all_scores <- my_scores |> 
  full_join(old_scores, by = join_by(VPNID == Subject, Language, Time))
all_scores
#' That seems to have worked.
#' 
#' 4. Depending on how you treated your data, you'll obtain a different
#' result here. But I get eight texts for which `total_score_arg` is `NA`,
#' but `Argumentation` isn't.
all_scores |> 
  filter(!is.na(Argumentation) & is.na(total_score_arg))
#' We can inspect the original data for all eight of these texts:
#' 
#' First entry (`AC_PLD_4`, Time 3, Portuguese): All non-`NA` values are 0
#' and `P3A_Nb_mots` reads `NA`. So this text was probably never written.
#' We'd need to check with the raw data (= the texts themselves) to be sure.
#' 
writing |> 
  filter(VPNID == "AC_PLD_4") |> 
  select(contains("P3A"))

#' Second entry (`AC_PLD_7`, Time 3, Portuguese): Exact same problem.
writing |> 
  filter(VPNID == "AC_PLD_7") |> 
  select(contains("P3A"))

#' Third entry (`AD_PLF_9`, Time 3, language of eduction): While the 
#' `Sco3A_Nb_mots` variable reads `NA`, several other values _are_ available.
#' So perhaps this time, the research assistant forgot to enter the text length?
#' Our assumption that texts with missing text lengths were never written seems
#' to have been wrong...
writing |> 
  filter(VPNID == "AD_PLF_9") |> 
  select(contains("Sco3A"))

#' We won't cover the other cases in detail, and we won't try to fix the remaining
#' problems here. Ideally, we'd go back to the raw data (i.e., the texts) to determine
#' which of these are truly missing and which aren't.
#' 
#' 5. Yes, 28 in my case. Depending on how you treated
#' the data, you'll have a different result here.
#' Since we don't know why the `Argumentation` data could have been missing,
#' we won't delve deeper into these discrepancies here.
all_scores |> 
  filter(is.na(Argumentation) & !is.na(total_score_arg))
#' 
#' 6. In 9 cases do we obtain slightly different argumentation scores.
#' These differences are likely related to the `MaxScore`
#' issue uncovered in Exercise 8.6.
differences <- all_scores |> 
  mutate(difference = Argumentation - total_score_arg) |> 
  filter(difference != 0)
differences
#' We can verify that for each of these participants, there is some
#' criterion score that exceeds the `MaxScore` for this criterion:
writing_long |> 
  mutate(Time = as.numeric(Time)) |> # leave out line to see what it does
  semi_join(differences) |> 
  select(VPNID, Language, Time, Criterion, score) |> 
  right_join(criteria_arg)
  
#' **Note:** In practice, it is usually better to not use `==`
#' and `!=` for numerical comparisons. Instead, use
#' the `all.equal()` function, as explained [here](https://gcdi.commons.gc.cuny.edu/2023/03/15/comparing-floating-point-numbers-in-r/).
#' 
#' # Exercise 11
#' 1. Section 9 outlines how we can reconstruct
#' the language and group information from the `VPNID` labels.
#' The combination of `Language` (`Sco` vs `P`) and `language_group`
#' is sufficient to figure out whether the language tested was
#' French, German or Portuguese.
all_scores <- all_scores |> 
  mutate(Group = str_split_i(VPNID, "_", 2)) |> 
  mutate(LanguageGroup = case_when(
    Group == "CP" ~ "Portuguese control",
    Group == "CF" ~ "French control",
    Group == "CD" ~ "German control",
    Group %in% c("PLF", "PNF") ~ "French-Portuguese",
    Group %in% c("PLD", "PND") ~ "German-Portuguese",
    .default =  "other"
  ))
#' We can use `mean()` and `sum()` to summarise
#' the results.
all_scores |> 
  group_by(LanguageGroup, Language, Time) |> 
  summarise(
    number_available = sum(!is.na(total_score_arg)),
    mean_arg = mean(total_score_arg, na.rm = TRUE),
    .groups = "drop"
  )
#'
#' 2. One option is as follows. We first make the `all_scores`
#' tibble wider by putting the different scores for each participant in each
#' language side by side. Then, we add a number of columns to the wider
#' tibble that specify if all three scores are available, if the T1 and T2 scores
#' are available but not the T3 score, etc. We then convert the dataset
#' to a longer format again and use `group_by()` and `summarise()`.
#' Finally, and optionally, we convert this summary to a wider format.
scores_wide <- all_scores |> 
  select(VPNID, LanguageGroup, Language, Time, total_score_arg) |> 
  pivot_wider(values_from = total_score_arg,
              names_from = Time,
              names_prefix = "T")
scores_wide # scores side by side
scores_available <- scores_wide |> 
  mutate(
    all_times = !is.na(T1) & !is.na(T2) & !is.na(T3),
    only_T1T2 = !is.na(T1) & !is.na(T2) & is.na(T3),
    only_T1T3 = !is.na(T1) & is.na(T2) & !is.na(T3),
    only_T2T3 = is.na(T1) & !is.na(T2) & !is.na(T3),
    only_T1 = !is.na(T1) & is.na(T2) & is.na(T3),
    only_T2 = is.na(T1) & !is.na(T2) & is.na(T3),
    only_T3 = is.na(T1) & is.na(T2) & !is.na(T3)
  ) |> 
  select(-T1, -T2, -T3) |> 
  pivot_longer(all_times:only_T3,
               names_to = "Time",
               values_to = "Flag")
scores_available # for each participant, exactly one `Flag` should read `TRUE`.
# Summarise and make wider
scores_available |> 
  group_by(LanguageGroup, Language, Time) |> 
  summarise(number = sum(Flag),
            .groups = "drop") |> 
  pivot_wider(names_from = Time, values_from = number)