# Setup ###########################################################################################

library(peakRAM)
library(pbapply)
library(tidyverse)

# Set default progress bar settings
pbo <- pbapply::pboptions()

# Constants controlling the experiment ------------------------------------------------------------
height <- 1e4
width <- 1e3
num_portions <- 20
step <- 1 / num_portions
portions <- seq(1 - step * (num_portions - 1), 1, step)
tmp_file_name <- "sample_data.csv"

# Create the sample data table --------------------------------------------------------------------
write.table(
  matrix(1:(height * width), height, width),
  file = tmp_file_name,
  row.names = FALSE,
  sep = ","
)

# List of column names and getting an event subset of them based on the portion of the columns we
# want to read.
col_names <- paste0("V", 1:width)
col_subset <- function(portion) {
  count <- portion * width
  step <- floor(1 / portion)
  col_names[seq(1, step * count, step)]
}

# Two methods of reading the data -----------------------------------------------------------------

# This one uses `col_select`
read_with_col_select <- function(portion)
  readr::read_csv(tmp_file_name,
                  col_select = any_of(col_subset(portion)),
                  show_col_types = FALSE)

# This one selects a subset of the table after loading the whole thing
read_then_select <- function(portion)
  readr::read_csv(tmp_file_name,
                  show_col_types = FALSE)[col_subset(portion)]

# Functions for benchmarking ----------------------------------------------------------------------

# Peak RAM usage measurement
measure_peak_ram <- function(expr)
  peakRAM::peakRAM(expr)$Peak_RAM_Used_MiB

# Execution time measurement
measure_exec_time <- function(expr) {
  start_time <- Sys.time()
  eval(expr)
  end_time <- Sys.time()
  as.numeric(end_time - start_time)
}

# Testing and Evaluating Results ##################################################################

# Generate tests ----------------------------------------------------------------------------------

# The number of times to repeat any combination of settings (reading method, portion)
repetition_count <- 5

# A collection of all reading methods to test
reading_functions <- c(read_with_col_select, read_then_select)

# Create a matrix of all combinations of portions and reading methods
alternating_reading_methods <-
  rep(1:length(reading_functions), length(portions))
repeated_portions <- rep(portions, each = length(reading_functions))
measurement_params <-
  cbind(alternating_reading_methods, repeated_portions)

# Repeat the values based on the constant for the number of repetitions and randomize the order
measurement_params <-
  do.call(rbind, rep(list(measurement_params), repetition_count))
measurement_params <-
  measurement_params[sample(nrow(measurement_params)),]

# Create an expression to be evaluated for each test
tests <- lapply(seq(nrow(measurement_params)), function(index) {
  read_file_expr <-
    bquote(reading_functions[[.(measurement_params[index, 1])]](.(measurement_params[index, 2])))
  bquote(c(
    .(index),
    .(measurement_params[index, 1]),
    .(measurement_params[index, 2]),
    measure_peak_ram(.(read_file_expr)),
    measure_exec_time(.(read_file_expr))
  ))
})

# Run tests ---------------------------------------------------------------------------------------
raw_mem_results <- pbapply::pblapply(tests, function(test) {
  eval(test)
})

# Processing results ------------------------------------------------------------------------------
mem_results <- do.call(rbind, raw_mem_results)
dimnames(mem_results)[[2]] <-
  c("id", "method", "portion", "memory", "exec_time")
mem_results <- as.data.frame(mem_results) %>%
  as_tibble() %>%
  mutate_at(vars(method), factor) %>%
  mutate(memory = memory / 1e6) %>%
  mutate(method = recode(method, `1` = "Optimized", `2` = "Default"))

# Plotting results --------------------------------------------------------------------------------

# Execution time
ggplot(mem_results, aes(x = portion, y = exec_time, color = method)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") +
  labs(x = "Portion of Columns Read", y = "Execution Time") +
  ggtitle("Execution Time vs. Portion of Columns Read")

# Memory usage
ggplot(mem_results, aes(x = portion, y = memory, color = method)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") +
  labs(x = "Portion of Columns Read", y = "Peak Memory Usage") +
  ggtitle("Peak Memory Usage vs. Portion of Columns Read")

# Remove files
remove_file <- function(f_name) {
  if (file.exists(f_name)) {
    file.remove(f_name)
  }
}

remove_file(file_name(width))
