
# Loading Data
# ============

# Unzip
unzip("activity.zip")
# Read .rds files
act <- read.csv("activity.csv")
# Check csv file
head(act)

act_fill <- act

for(v in act_fill){
        print(v)
}