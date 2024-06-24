# Small snippet to query the family and order for a given species name in binomial format. 
# Vinay K L 24/06/2024

# Load libraries
library(taxize)
library(readr)

# File path for input list of species downloaded from NCBI genomes list #1019
file_path <- "../data/eukaryotes-2.csv"
data <- read_csv(file_path)

# Functions to call family and order names using taxize with ncbi database as target
get_family_order <- function(species_name) {
  tryCatch({
    classification_data <- classification(species_name, db = "ncbi")
    family <- classification_data[[1]] %>%
      filter(rank == "family") %>%
      select(name) %>%
      pull()
    order <- classification_data[[1]] %>%
      filter(rank == "order") %>%
      select(name) %>%
      pull()
    return(data.frame(Family = family, Order = order))
  }, error = function(e) {
    return(data.frame(Family = NA, Order = NA))
  })
}

# Loop over the species names under the dataframe for each rows and store them as vector
results <- t(apply(data, 1, function(row) {
  species_name <- row["#Organism Name"]
  family_order <- get_family_order(species_name)
  c(as.character(row), as.character(family_order$Family), as.character(family_order$Order))
}))

# Convert the vector to obtain the seperate database
results_df <- as.data.frame(results, stringsAsFactors = FALSE)

# Rename the columns
colnames(results_df) <- c(colnames(data), "Family", "Order")

# Merge the original database with results database
final_data <- cbind(data, results_df[, c("Family", "Order")])

# Just checking the results. 
print(final_data)

# Save the database as CSV
write.csv(final_data, file = "../output/bird_genomes_with_fam_order.csv")
