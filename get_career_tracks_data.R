download.file("http://shr.ucsc.edu/policy/special-projects/career-tracks/Resources/Career%20Tracks%20Job%20Function%20Summary.xlsx",
              destfile = "job_families_functions_descriptions.xlsx")
download.file("http://shr.ucsc.edu/policy/special-projects/career-tracks/Resources/Career%20Tracks%20Job%20Title%20Listing%202-15-17.pdf",
              destfile = "career_tracks_info.pdf")
download.file("http://shr.ucsc.edu/policy/special-projects/career-tracks/Resources/Salary%20Range%20Structure_July2016.pdf",
              destfile = "career_salary_grades.pdf")

job_families_functions <- readxl::read_excel("job_families_functions_descriptions.xlsx")
career_tracks <- tabulizer::extract_tables("./career_tracks_info.pdf")
salaries <- tabulizer::extract_tables("./career_salary_grades.pdf")

career_tracks_df <- career_tracks %>% 
  map_df(data.frame)
header_df <- career_tracks_df %>% 
  slice(1) %>% 
  as.character()
names(career_tracks_df) <- header_df

salaries <- salaries %>% 
  map_df(data.frame)
salaries <- salaries %>% 
  slice(-1) %>% 
  mutate_all(as.character)
salaries_header <- salaries %>% 
  slice(1) %>% 
  as.character()
names(salaries) <- salaries_header
salaries <- salaries %>% 
  slice(-1) %>% 
  janitor::clean_names()

job_families_functions <- janitor::clean_names(job_families_functions)

coerce_numeric <- function(x) {
  s <- gsub(",", "", x)
  as.numeric(s)
}
career_tracks <- career_tracks_df %>%
  janitor::clean_names() %>% 
  filter(job_family != "Job Family") %>% 
  left_join(salaries, by = "grade") %>% 
  left_join(job_families_functions, by = c("job_family", "job_function")) %>% 
  mutate_at(vars(matches("minimum|midpoint|maximum")), coerce_numeric)


saveRDS(career_tracks, file = "career_tracks.RDS")
ggplot(career_tracks, aes(as.numeric(maximum))) +
  geom_histogram() +
  scale_x_continuous(labels = dollar)