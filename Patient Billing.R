library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)
library(here)


setwd('C:/Users/timot/OneDrive/Augustana/Junior year/Spring semester/DATA 332/Data')

Billing <- read_xlsx('Billing.xlsx')

Patient <- read_xlsx('Patient.xlsx')

Visit <- read_xlsx('Visit.xlsx')


Visit_Patient_Join <- left_join(Visit, Patient, by = 'PatientID')

Billing_visit_Patient_Join <- left_join(Billing, Visit_Patient_Join, by = 'VisitID')

Visit_Patient_Join <- Visit_Patient_Join %>%
  mutate(NumericMonth = month(as.Date(VisitDate)),
         MonthName = recode(NumericMonth,
                            `1` = "January", `2` = "February", `3` = "March",
                            `4` = "April", `5` = "May", `6` = "June",
                            `7` = "July", `8` = "August", `9` = "September",
                            `10` = "October", `11` = "November", `12` = "December"))

reason_summary <- Visit_Patient_Join %>%
  group_by(Reason, MonthName) %>%
  summarize(count_per_reason = n())

ggplot(Visit_Patient_Join, aes(x = MonthName, fill = Reason)) +
  geom_bar() +
  labs(title = "Reason for Visit Segmented by Month",
       x = "Month",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  

ggsave(here("figures", "reason_per_month.png"))


reason_based_on_walkin <- Visit_Patient_Join %>%
  group_by(WalkIn, Reason) %>%
  summarize(count_per_reason = n())

reason_based_on_zip <- Visit_Patient_Join %>%
  group_by(Zip, Reason) %>%
  summarize(count_per_reason = n())

invoice_summary <- Billing_visit_Patient_Join %>%
  group_by(Reason, InvoicePaid) %>%
  summarize(count_per_reason = n(),
            Total_invoice = sum(InvoiceAmt))

ggplot(invoice_summary, aes(x = Reason, y = Total_invoice, fill = InvoicePaid)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Total Invoice Amount Based on Reason for Visit",
       x = "Reason for Visit",
       y = "Total Invoice Amount",
       fill = "Invoice Paid") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8))

ggsave(here("figures", "Total_Invoice_per_Reason.png"))


invoice_item_summary <- Billing_visit_Patient_Join %>%
  group_by(InvoiceItem) %>%
  summarize(count_per_item = n())

ggplot(invoice_item_summary, aes(x = count_per_item, y = InvoiceItem)) +
  geom_bar(stat = "identity", width = 0.5, fill = "steelblue") +
  labs(title = "Count of Invoice Items",
       x = "Count",
       y = "Invoice Item") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust=1, size = 8)) +
  coord_flip()

ggsave(here("figures", "Count_per_Invoice_Item.png"))


