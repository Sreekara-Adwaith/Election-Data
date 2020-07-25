# Calling the required libraries

library(tidyverse)
library(readxl)
library(xlsx)
library(writexl)

# Reading the data

data_ge <- read_csv("Election Data/ge.all.20190729.csv")  # (Why is as_tibble not working?)
View(data_ge)

### The literacy data has been imported using built in import

### The data sheet for the combined values has also been imported using the built in import


# Cleaning the election data

clean <- data_ge %>%
  select(State_Name, Constituency_No, Votes, Margin, Position, Assembly_No) %>%
  drop_na(Votes, Margin)

View(clean)


# Cleaning the Literacy Data

lit_clean <- Election_literacy_project %>%
  select(State_Name, Year, Total) %>%
  drop_na(Total)

lit_clean$Total = as.numeric(lit_clean$Total) / 100


# Cleaning the combined data

plot <- Combined %>%
  select(State_Name, Lok_Sabha, Margin_Percent, Literacy_Rate) %>%
  drop_na(Literacy_Rate) %>%
  drop_na(Margin_Percent)

plot$Literacy_Rate = as.numeric(plot$Literacy_Rate) / 100

plot <- drop_na(plot, Literacy_Rate)

# Creating a data frame with just the names of the states and the union territories

states <- data_ge %>%
  filter(Constituency_No == 1, Assembly_No == 17, Position == 1) %>%
  select(State_Name)

View(states)


## Creating Functions

# Margin per assembly

per_assembly <- function(data, assembly_no) {
  wip1 <- filter(data, Assembly_No == assembly_no)
  wip2 <- filter(wip1, Position == 1)
  a = sum(wip1$Votes)
  b = sum(wip2$Margin)
  return(b/a)
}


# Margin per state

per_state <- function(data, state_name) {
  wip1 <- filter(data, State_Name == state_name)
  wip2 <- filter(wip1, Position == 1)
  a <- sum(wip1$Votes)
  b <- sum(wip2$Margin)
  return(b/a)
}


# Margin per state and per assembly

per_state_per_assembly <- function(data, state_name, assembly_no) {
  wip1 <- filter(data, State_Name == state_name, Assembly_No == assembly_no,)
  wip2 <- filter(wip1, Position == 1)
  a <- sum(wip1$Votes)
  b <- sum(wip2$Margin)
  return(b/a)
}


# Margin for a the states per assembly

state_per_assembly <- function(data, state_name) {
  vector <- c()
  for (i in unique(data$Assembly_No)) {
    vector <- append(vector, per_state_per_assembly(data, state_name, i))
  }
  return(vector)
}


# Margin for an assembly per state

assembly_per_state <- function(data, assembly_no) {
  vector <- c()
  for (i in unique(data$State_Name)) {
    vector <- append(vector, per_state_per_assembly(data, i, assembly_no))
  }
  return(vector)
}


# Function to create a matrix with all the margins

all_margins <- function(data) {
  margins <- matrix(nrow = 0, ncol = 15)
  for (i in unique(data$State_Name)) {
    margins <- rbind(margins, state_per_assembly(data, i))
  }
  return(margins)
}


# Function to create a vector of all the state names

state_names <- function(data) {
  vector <- c()
  for (i in unique(data$State_Name)) {
    vector <- append(vector, i)
  }
  return(vector)
}


# Function to create a vector of all the assembly numbers

assemblies <- function(data) {
  vector <- c()
  for (i in unique(data$Assembly_No)) {
    vector <- append(vector, i)
  }
  return(as.character(vector))
}

# Function to create a data frame with all the margins and the right names

data_frame <- function(data) {
  margins_df <- as.data.frame(all_margins(data))
  colnames(margins_df) <- paste(assemblies(data))
  margins_df <- cbind(State_Name = state_names(clean), margins_df)
  return(margins_df)
}

margins_wide <- data_frame(clean)


# Trying to gather the data

plot_ge <- gather(margins_wide, Lok_Sabha, Margin_Percent, -1)

plot_ge$Lok_Sabha <- as.numeric(as.character(plot_ge$Lok_Sabha))

write.xlsx(plot_ge, "Plot_Data.xlsx")

View(plot_ge)


# Trying to plot the data


k <- filter(plot_ge, State_Name == "Kerala")

ggplot(plot_ge, aes(x = Lok_Sabha, y = Margin_Percent, group = State_Name)) +
  geom_line(color="darkred")


# Function to plot the required state for the margins

plot_per_state <- function(data, state_name) {
  state_data <- filter(data, State_Name == state_name)
  plot <- ggplot(state_data, aes(x = Lok_Sabha, y = Margin_Percent, group = State_Name)) +
    geom_line(colour = "red") +
    ylim(0, 0.5)
  return(plot)
}


# Trying to plot the required state for the literacy rate

lit_per_state <- function(data, state_name) {
  state_data <- filter(data, State_Name == state_name)
  plot <- ggplot(state_data, aes(x = Year, y = Total, group = State_Name)) +
    geom_line(colour = "blue")
  return(plot)
}



# Function to plot the victory margins for 2 states at the same time

plot_states <- function(data, state_name1, state_name2) {
  state_data1 <- filter(data, State_Name == state_name1)
  state_data2 <- filter(data, State_Name == state_name2)
  plot <- ggplot(state_data1, aes(Margin_Percent))+
    geom_line(data = state_data1, aes(x = Lok_Sabha, y = Margin_Percent, group = state_name1), colour = "red") +
    geom_line(data = state_data2, aes(x = Lok_Sabha, y = Margin_Percent, group = state_name2), colour = "blue")
  return(plot)
}


# Function to plot the victory margin and the literacy rate

plot_together <- function(data_margin, data_literacy, state_name) {
  data_marg <- filter(data_margin, State_Name == state_name)
  data_lit <- filter(data_literacy, State_Name == state_name)
  plot <- ggplot(data_marg, aes(Margin_Percent))+
    geom_line(data = data_marg, aes(x = Lok_Sabha, y = Margin_Percent, group = state_name), colour = "red") +
    geom_line(data = data_lit, aes(x = Year, y = Total, group = state_name), colour = "blue")
  return(plot)
}


# Playing around with the scatter plot data


attempt1 <- filter(plot, Lok_Sabha == 16)

plot1 <- ggplot(attempt1, aes(x = Margin_Percent, y = Literacy_Rate ,group = State_Name)) +
  geom_point()
ggsave("plot1.png")


# Function for scatter plot

scatter_plot <- function(data, lok_sabha) {
  plot_data <- filter(data, Lok_Sabha == lok_sabha)
  plot <- ggplot(plot_data, aes(x = Margin_Percent, y = Literacy_Rate)) +
    geom_point() +
    stat_smooth(method = "lm", se = FALSE)
  return(plot)
}


## Trying to write a function to save these plots

# For the victory margin plots

save_margins <- function(data, state_name) {
  state_data <- filter(data, State_Name == state_name)
  plot <- ggplot(state_data, aes(x = Lok_Sabha, y = Margin_Percent, group = State_Name)) +
    geom_line(colour = "red") +
    ylim(0, 0.5) +
    xlim(3, 17) +
    labs(y = "Victory Margin (In Percentage)", x = "Lok Sabha Assembly Number") +
    ggtitle(state_name)
  ggsave(paste0("Margins; ", state_name, ".png"))
  return(plot)
}


# For the literacy rate

save_literacy_rate <- function(data, state_name) {
  state_data <- filter(data, State_Name == state_name)
  plot <- ggplot(state_data, aes(x = Year, y = Total, group = State_Name)) +
    geom_line(colour = "blue") +
    ylim(0, 1) +
    xlim(1951, 2011) +
    labs(y = "Literacy Rate") +
    ggtitle("Literacy Rate ", state_name)
  ggsave(paste0("Literacy Rate; ", state_name, ".png"))
  return(plot)
}


# For the scatter plots, saving them with the regression line.

save_scatter_plot <- function(data, lok_sabha) {
  plot_data <- filter(data, Lok_Sabha == lok_sabha)
  plot <- ggplot(plot_data, aes(x = Margin_Percent, y = Literacy_Rate)) +
    geom_point() +
    geom_smooth(method = "lm") +
    ylim(0, 1) +
    xlim(0, 1) +
    labs(y = "Total Literacy Rate", x = "Margin Of Victory (in percentage)") +
    geom_smooth(method = "lm", se = FALSE) +
    ggtitle(paste0("Lok Sabha ", lok_sabha))
  ggsave(paste0("Scatter Plot; Lok Sabha ", lok_sabha, ".png"))
  return(plot)
}


## Trying to loop the function to save all of them together


# For the scatter plots

for (i in (unique(plot$Lok_Sabha))) {
  save_scatter_plot(plot, i)
}


# For the margin percentage per state

for (i in (unique(plot_ge$State_Name))) {
  save_margins(plot, i)
}


# For the literacy rate

for (i in (unique(lit_clean$State_Name))) {
  save_literacy_rate(plot, i)
}


# Trying to get the regression co-efficients






























