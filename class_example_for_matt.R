extract_GVA <- function(x) {
  df <- data.frame(col1 = x)
  if (!identical(1:5, df$col1)) stop("GVA data not 1 to 5")
  structure(
    df,
    class = c("GVA", "data.frame"))
}

GVA <- extract_GVA(1:4) #throws error if data incorrect
GVA <- extract_GVA(1:5) #creates dataframe with GVA class that appears in
# data section of global environment tab


extract_ABS <- function(x) {
  df <- data.frame(col1 = x)
  if (!identical(6:10, df$col1)) stop("ABS data not 6 to 10")
  structure(
    df,
    class = c("ABS", "data.frame"))
}

#this function also stores some attributes
combine_GVA <- function(ABS, GVA) {
  if(!identical(class(ABS), c("ABS", "data.frame")) |
     !identical(class(GVA), c("GVA", "data.frame"))) stop("datasets wrong class")

  structure(
    rbind(ABS, GVA),
    years = 3:8,
    class = c("combine_GVA", "data.frame")
  )
}

combined_GVA <- combine_GVA(ABS = extract_ABS(6:10), GVA = extract_GVA(1:5))
attributes(combined_GVA)$years #access attributes

#fails unless data has been checked and therefore has correct class
class(GVA) = "other"
combined_GVA <- combine_GVA(ABS = extract_ABS(6:10), GVA = GVA)

class(combined_GVA[combined_GVA$col1 %in% attributes(combined_GVA)$years, ]) #access attributes

#using attributes in data cleaning
subset_GVA <- function(df) {
  if(!identical(class(df), c("combine_GVA", "data.frame"))) stop("dataset has wong class")

  df <- data.frame(col1 = df[df$col1 %in% attributes(df)$years, ])
  structure(
    df,
    class = c("subset_GVA", "data.frame")
  )
}

subsetted_GVA <- subset_GVA(combined_GVA)

#other functions that would be used - allows intermediate datasets to be easily used
overlap <- overlap(combined_GVA)
GVA_summary <- summary_table(subsetted_GVA)

#can also produce output without creating any intermediate datasets
rm(GVA, combined_GVA, subsetted_GVA)
output <-
  subset_GVA(
    combine_GVA(
      ABS = extract_ABS(6:10),
      GVA = extract_GVA(1:5)
    )
  )

