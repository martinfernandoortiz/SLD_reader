
#Secuencia para agregar registros por n'umero de escala

# Create the original table
original_table <- data.frame(
  field1 = c("a", "b", "c", "d"),
  min = c(1, 2, 5, 7),
  max = c(3, 6, 5, 12),
  number = NA
)

# Create an empty dataframe to store the new rows
new_rows <- data.frame(field1 = character(), number = numeric())

# Loop through each row in the original table
for (i in seq_len(nrow(original_table))) {
  # Calculate the difference between min and max
  diff <- original_table$max[i] - original_table$min[i]
  
  # Generate a sequence of numbers from min to max
  numbers <- seq(from = original_table$min[i], to = original_table$max[i])
  
  # Create a new data frame with field1 and number columns
  new_row <- data.frame(
    field1 = rep(original_table$field1[i], diff + 1),
    number = numbers
  )
  
  # Add the new row(s) to the empty dataframe
  new_rows <- rbind(new_rows, new_row)
}

# Print the original table and the new rows
print(original_table)
print(new_rows)
