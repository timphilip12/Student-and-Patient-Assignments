library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)

setwd('C:/Users/timot/OneDrive/Augustana/Junior year/Spring semester/DATA 332/Data')

Courses <- read_xlsx('Course.xlsx')

Registrations <- read_xlsx('Registration.xlsx')

Students <- read_xlsx('Student.xlsx')

Student_registration_lj <- left_join(Students, Registrations, by = 'Student ID')

Student_registration_course_lj <- left_join(Student_registration_lj, Courses, by = 'Instance ID')

course_cost_summary <- Student_registration_course_lj %>%
  group_by(`Instance ID`, Title, `Start Date`) %>%
  summarize(count_student_per_course = n()
  )

course_cost_plan_summary <- Student_registration_course_lj %>%
  group_by(`Payment Plan`, `Instance ID`, Title) %>%
  summarize(total_balance_due = sum(`Balance Due`, na.rm = TRUE),
            total_cost_per_course = sum(`Total Cost`, na.rm = TRUE))

ggplot(course_cost_plan_summary, aes(x = Title, y = total_cost_per_course, fill = `Payment Plan`)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Total Cost per Major, Segmented by Payment Plan",
       x = "Major",
       y = "Total Cost",
       fill = "Payment Plan") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(course_cost_plan_summary, aes(x = Title, y = total_balance_due, fill = `Payment Plan`)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Total balance per Major, Segmented by Payment Plan",
       x = "Major",
       y = "Total Balance",
       fill = "Payment Plan") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Define the colors you want to use
my_colors <- c("#0073C2", "#EFC000", "#868686", "#CD534C", "#7EA7D8", "#00FF00", "#FFD700", "#8A2BE2")

# Create the plot
ggplot(course_cost_summary, aes(x = `Instance ID`, y = count_student_per_course, fill = Title)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = my_colors) +
  labs(title = "Count of Students per Course",
       x = "Course ID",
       y = "Number of Students") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here("figures", "count_students.png"))

Students$Birth_Year <- year(ymd(Students$`Birth Date`))

birth_year_summary <- Students %>%
  group_by(Birth_Year) %>%
  summarize(count_student_per_year = n())

ggplot(birth_year_summary, aes(x = Birth_Year, y = count_student_per_year, fill = Birth_Year)) +
  geom_col() +
  labs(title = "Count of Students per Birth Year",
       x = "Birth Year",
       y = "Number of Students") +
  theme_minimal()

ggsave(here("figures", "count_students_birth.png"))
