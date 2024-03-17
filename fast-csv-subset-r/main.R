# Setup

library(peakRAM)
library(pbapply)
library(tidyverse)

pbo <- pbapply::pboptions()

height <- 1e4
width <- 1e3
num_portions <- 5
step <- 1 / num_portions
portions <- seq(1 - step * (num_portions - 1), 1, step)

col_names <- function(width)
  paste0("C", 1:width)

col_subset <- function(width, portion) {
  count <- portion * width
  step <- floor(1 / portion)
  col_names(width)[seq(1, step * count, step)]
}

file_name <- function(width)
  paste0("sample-data_", width, ".csv")

write_file <- function(width)
  write.table(
    matrix(1:(height * width), height, width),
    file = file_name(width),
    row.names = FALSE,
    col.names = FALSE,
    sep = ","
  )

read_file_method_1 <- function(portion)
  readr::read_csv(
    file_name(width),
    col_names = col_names(width),
    col_select = col_subset(width, portion),
    show_col_types = FALSE
  )

read_file_method_2 <- function(portion)
  readr::read_csv(file_name(width),
                  col_names = col_names(width),
                  show_col_types = FALSE)[col_subset(width, portion)]

read_file <- function(method_num, portion)
  read_methods[[method_num]](portion)

read_methods <- c(read_file_method_1, read_file_method_2)
num_methods <- length(read_methods)

measure_peak_ram <- function(expr)
  peakRAM::peakRAM(expr)$Peak_RAM_Used_MiB

measure_exec_time <- function(expr) {
  start_time <- Sys.time()
  eval(expr)
  end_time <- Sys.time()
  as.numeric(end_time - start_time)
}

# Generate files
write_file(width)

# 3. analyze the data (hypothesis testing)

# Generate tests
num_rep <- 5
measurement_params <-
  cbind(rep(1:(num_methods), length(portions)), rep(portions, each = num_methods))
measurement_params <-
  do.call(rbind, rep(list(measurement_params), num_rep))
measurement_params <-
  measurement_params[sample(nrow(measurement_params)), ]
tests <- lapply(seq(nrow(measurement_params)), function(index) {
  read_file_expr <-
    bquote(read_file(.(measurement_params[index, 1]), .(measurement_params[index, 2])))
  bquote(c(
    .(index),
    .(measurement_params[index, 1]),
    .(measurement_params[index, 2]),
    measure_peak_ram(.(read_file_expr)),
    measure_exec_time(.(read_file_expr))
  ))
})

# Run tests
raw_mem_results <- pbapply::pblapply(tests, function(test) {
  eval(test)
})

# Processing results
mem_results <- do.call(rbind, raw_mem_results)
dimnames(mem_results)[[2]] <-
  c("id", "method", "portion", "memory", "exec_time")
mem_results <- as_tibble(as.data.frame(mem_results))
mem_results <- mem_results %>%
  mutate_at(vars(method), factor) %>% 
  mutate(memory = memory / 1e6) %>% 
  mutate(method = recode(method, `1` = "Optimized", `2` = "Default"))

# Plotting results

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
