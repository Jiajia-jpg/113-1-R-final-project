# Load the googlesheets4 package
library(googlesheets4)

# Google Sheet URL
sheet_url <- "https://docs.google.com/spreadsheets/d/1R3PHHHA3f40X0oLpcTQ_Md6TwNNgLfugRY0vpdTRYWQ/edit?gid=0#gid=0"

# Read the first sheet (default) or specify the sheet by name or index
df <- read_sheet(sheet_url)

# View the data
print(df)
