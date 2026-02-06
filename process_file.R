# process_file.R
# Read a SAS census file and reshape from wide to long; parse demographic code fields;
# then aggregate population by ST, Year, age_group, Ethnicity, Race, Sex.
#
# Expects a wide-format SAS census file, where population counts are stored in separate 
# columns for sex–race/ethnicity combinations and each row corresponds to a unique state, year, and age.
#
# Dependencies (based on calls): haven, dplyr, tidyr, stringr

process_file <- function(path) {
    df <- haven::read_sas(path)

    df <- df %>%
        select(-any_of(c("StFIP", "CoFip", "AgeGroup"))) %>%
        pivot_longer(
            cols = -(1:5),
            names_to = "demo_code",
            values_to = "population"
        ) %>%
        mutate(
            Ethnicity = if_else(str_sub(demo_code, 1, 1) == "N", "Non-Hispanic", "Hispanic"),
            Race = case_when(
                str_detect(demo_code, "W") ~ "White",
                str_detect(demo_code, "B") ~ "Black",
                str_detect(demo_code, "I") ~ "American Indian or Alaska Native",
                str_detect(demo_code, "A") ~ "Asian",
                str_sub(demo_code, -2, -2) == "H" ~ "Native Hawaiian or Other Pacific Islander",
                str_detect(demo_code, "O") ~ "Other",
                str_detect(demo_code, "T") ~ "Two or More Races",
                TRUE ~ NA_character_
            ),
            Sex = case_when(
                str_detect(demo_code, "M") ~ "Male",
                str_detect(demo_code, "F") ~ "Female",
                TRUE ~ NA_character_
            )
        ) %>%
        select(1:6, Ethnicity, Race, Sex, population)%>%
        mutate(age=Age)

    # Age grouping if 'age' exists
    if ("age" %in% names(df)) {
        df <- df %>%
            mutate(age=as.numeric(as.character(age)))%>%
            mutate(age_group = case_when(
                age < 1 ~ "< 1 Yr",
                age >= 1 & age <= 4 ~ "1-4 Yrs",
                age >= 5 & age <= 17 ~ "5-17 Yrs",
                age >= 18 & age <= 59 ~ "18-59 Yrs",
                age >= 60 ~ ">=60 Yrs",
                TRUE ~ NA_character_
            )) %>%
            select(-age)
    } else {
        df <- df %>%
            mutate(age_group = NA_character_)
    }

    # ✅ Aggregate *before* returning
    df %>%
        group_by(ST, Year, age_group, Ethnicity, Race, Sex) %>%
        dplyr::summarise(population = sum(population, na.rm = TRUE), .groups = "drop")
}
