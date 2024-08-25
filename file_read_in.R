setwd("C:/Users/dougl/Downloads/test")

library(dplyr)
library(purrr)
library(fs)
library(zip)
library(tools)
library(readr)
library(data.table)
library(janitor)

#reading in CSV file and add a column with the file name without .csv at the end
read_csv_file <- function(file_path) {
    cat("Reading file:", file_path, "\n")
    tryCatch({
      data <- read.csv(file_path, stringsAsFactors = FALSE)
      data$source_file <- sub("//.csv$", "", basename(file_path))
      return(data)
    }, error = function(e) {
      cat("Error reading file:",
    file_path, "\n")
      NULL
    })
  
}

#unzipping files
unzip_files <- function(zip_file, extract_to) {
  cat("Unzipping file:", zip_file, "to", extract_to, "\n")
  unzip(zip_file, exdir = extract_to)
}

#defining main directory path
main_dir <- "C:/Users/dougl/Downloads/test"
temp_dir <- file.path(main_dir, "temp_unzipped")

#debug print temp_dir to check its values
cat("Temporary directory path:", temp_dir, "\n")

#create temporary directory for the unzipped files
if (!dir.exists(temp_dir)) {
  dir.create(temp_dir, showWarnings = FALSE)
             cat("Temporary directory already exists:", temp_dir, "\n")
}

#list all zip files in the directory and sub directories
zip_files <- dir_ls(main_dir, regexp =  "\\.zip$", recurse = TRUE)

#debug print the zip files found
cat("Zip files found:\n")
print(zip_files)

#unzip all zip files to the temporary directory
walk(zip_files, ~unzip_files(.x, temp_dir))

############################################################################
############################################################################
y = list.files(temp_dir,pattern = ".*.txt")

data <- lapply(paste(temp_dir, y, sep = "/"), function(x) read.table(x, text = (
  "Element1 Element2 Element3 Element4 Element5 Element6 Element7 Element8"),
  sep = "\t"))

## to remove the horrible list/dataframe format

data1 <- data %>% reduce(full_join)

## To select the business names

library(stringr)
out <- data1 %>%
  filter(str_detect(V1, "Business Name: "))

colnames(out)[1] <- "name"

#########################################################################
#######################################################################
#list all csv files in the temporary directory
csv_files <- dir_ls(temp_dir, regexp = "\\.csv$", recurse = TRUE)

#Print out the csv files that were found debugged
cat("CSV files found:\n")
print(csv_files)


#reading and combining all the csv files into one df
i <- csv_files %>%
  lapply(read.csv) %>% 
  sapply(as.character) %>%
   write.csv("i.csv", row.names = TRUE)

i1 <- colnames(read_csv(csv_files[1]))

combined_df <-  read_csv("i.csv")
  

combined_data <- setNames(as.data.frame(t(combined_df[-1])), i1) 

rownames(combined_data) <- sub('.csv', '', rownames(combined_data))
rownames(combined_data) <- sub(temp_dir, '', rownames(combined_data))
rownames(combined_data) <- sub('/', '', rownames(combined_data))

combined_data <-   bind_cols(combined_data, out) %>%
  tibble::rownames_to_column()

combined_data1 <- as.data.frame(combined_data[,2:12])
combined_data2 <- as.data.frame(combined_data[,1]) 
colnames(combined_data2)[1] <- "file"

combined_data <- bind_cols(combined_data1, combined_data2)

combined_data$`Percentage of Nicotine` <- 
  gsub("%", "", as.character(combined_data$`Percentage of Nicotine`))

combined_data$`Percentage of Nicotine` <- 
  as.numeric(combined_data$`Percentage of Nicotine`)

####################################################
####################################################

#Makes sure files are successfully read
missing_files <- csv_files[sapply(combined_data, is.null)]
if (length(missing_files) >0) {
  cat("The following files were not successfully read:\n")
  print(missing_files)
}

#define output file path
output_csv <- "combined_data.csv"

#write the combined data to single csv file
write.csv(combined_data, output_csv, row.names = TRUE)

#print message indicating success
cat("All files have been read and combined", output_csv, "\n")

file.remove('i.csv') 

#remove temporary unzip file
unlink(temp_dir, recursive = TRUE)




 





