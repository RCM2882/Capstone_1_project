install.packages("ggplot2")
install.packages("dplyr")

library(ggplot2)
library(dplyr)
library(readr)
library(scales)

Consumer_Complaints <- read_csv("/Users/rcm/Documents/CapstoneProject/Consumer_Complaints.csv")

str(Consumer_Complaints)

head(Consumer_Complaints)

summary(Consumer_Complaints)

colnames(Consumer_Complaints)

colnames(Consumer_Complaints) <- gsub("\"", "", colnames(Consumer_Complaints))
colnames(Consumer_Complaints) <- gsub(" ", "_", colnames(Consumer_Complaints))

colnames(Consumer_Complaints)

Consumer_Complaints$Date_received <- as.Date(Consumer_Complaints$Date_received, format = "%m/%d/%Y")
str(Consumer_Complaints)

complaints_by_date <- Consumer_Complaints %>%
  group_by(Date_received) %>%
  summarise(count = n())

head(complaints_by_date)

ggplot(complaints_by_date, aes(x = Date_received, y = count)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "green", linetype = "dashed") +
  labs(title = "Number of Consumer Complaints Over Time",
       x = "Date",
       y = "Number of Complaints") +
  scale_x_date(labels = date_format("%Y-%m"), breaks = "6 months") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
)


complaints_by_product <- Consumer_Complaints %>%
  group_by(Product) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

ggplot(complaints_by_product, aes(x = count, y = reorder(Product, -count), fill = Product)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(title = "# of Complaints by Product",
       x = "Number of Complaints",
       y = "Product") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

Consumer_Complaints$Month <- format(Consumer_Complaints$`Date_received`, "%m")

complaints_by_month <- Consumer_Complaints %>%
  group_by(Month) %>%
  summarise(count = n()) %>%
  arrange(as.numeric(Month))

ggplot(complaints_by_month, aes(x = Month, y = count, fill = Month)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Number of Complaints by Month",
       x = "Month",
       y = "Number of Complaints") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

complaints_by_company <- Consumer_Complaints %>%
  group_by(Company) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(10, count)

ggplot(complaints_by_company, aes(x = count, y = reorder(Company, -count), fill = Company)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(title = "Number of Complaints by Company",
       x = "Number of Complaints",
       y = "Company") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(angle = 75, hjust = 1)
  )


real_estate_keywords <- c('Real_Estate', 'Broker', 'Agency')
real_estate_complaints <- Consumer_Complaints %>%
  filter(grepl(paste(real_estate_keywords, collapse = "|"), Product, ignore.case = TRUE) |
           grepl(paste(real_estate_keywords, collapse = "|"), Issue, ignore.case = TRUE))

complaints_by_date <- real_estate_complaints %>%
  group_by(Date_received) %>%
  summarise(count = n())

ggplot(complaints_by_date, aes(x = Date_received, y = count)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 1) +
  geom_smooth(method = "loess", se = FALSE, color = "green", linetype = "dashed") +
  labs(title = "Real Estate Agencies and Brokers Over Time",
       x = "Date",
       y = "Number of Complaints") +
  scale_x_date(labels = scales::date_format("%Y-%m"), breaks = scales::date_breaks("6 months")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
  
Consumer_Complaints$Date_received <- as.Date(Consumer_Complaints$Date_received, format = "%m/%d/%Y")

mortgage_complaints <- Consumer_Complaints %>%
  filter(Product == "Mortgage")

head(mortgage_complaints)

mortgage_complaints$Year <- format(mortgage_complaints$Date_received, "%Y")

mortgage_complaints_by_year <- mortgage_complaints %>%
  group_by(Year) %>%
  summarise(count = n())

ggplot(mortgage_complaints_by_year, aes(x = Year, y = count, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 2) +
  labs(title = "Mortgage-Related Complaints Over Time",
       x = "Year",
       y = "Number of Complaints") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

loan_related_keywords <- c('Loan modification', 'Collection', 'Foreclosure')
loan_related_complaints <- Consumer_Complaints %>%
  filter(grepl(paste(loan_related_keywords, collapse = "|"), Issue, ignore.case = TRUE))

loan_related_complaints_by_year <- loan_related_complaints %>%
  mutate(Year = format(Date_received, "%Y")) %>%
  group_by(Year) %>%
  summarise(count = n())

ggplot(loan_related_complaints_by_year, aes(x = Year, y = count, group = 1)) +
  geom_line(color = "green") +
  geom_point(color = "purple", size = 2) +
  labs(title = "Loan Modification, Collection, and Foreclosure",
       x = "Year",
       y = "Number of Complaints") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
