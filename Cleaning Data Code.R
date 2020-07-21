# Calling the required libraries

library(tidyverse)

# Reading the data

data_ge <- read_csv("Election Data/ge.all.20190729.csv")  # (Why is as_tibble not working?)
View(data_ge)


# Cleaning the data

clean <- data_ge %>%
  select(State_Name, Constituency_No, Votes, Margin, Position, Assembly_No) %>%
  drop_na(Votes, Margin)

View(clean)

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

plot_data_ge <- gather(margins_wide, Lok_Sabha, Margin_Percent, -1)

plot_data_ge$Lok_Sabha <- as.numeric(as.character(plot_data_ge$Lok_Sabha))

View(plot_data_ge)


# Trying to plot the data


k <- filter(plot_data_ge, State_Name == "Kerala")

ggplot(plot_data_ge, aes(x = Lok_Sabha, y = Margin_Percent, group = State_Name)) +
  geom_line(color="darkred")


# Function to plot the required state

plot_per_state <- function(data, state_name) {
  state_data <- filter(data, State_Name == state_name)
  plot <- ggplot(state_data, aes(x = Lok_Sabha, y = Margin_Percent, group = State_Name)) +
    geom_line(color = "red")
  return(plot)
}




plot_states <- function(data, state_name1, state_name2) {
  state_data <- filter(data, State_Name == state_name1)
  plot <- ggplot(state_data, aes(x = Lok_Sabha, y = Margin_Percent, group = State_Name)) +
    ggplot(state_data, aes(x = Lok_Sabha, y = Margin_Percent, group = State_Name))
  geom_line(color = "red")
  return(plot)
}