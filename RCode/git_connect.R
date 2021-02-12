### ### ### ### ### ###
####  Push to Git  ####
### ### ### ### ### ###
# Principal Author: Timothy Model
# January 20, 2020

#### Load packages ----

library(git2r)

##### Commit & Push Private Repository to Github ----

# Ensure that git is connected to R
# If not connected, configure git with
# git2r::config(user.name = "myusername", user.email = "myemail")
git2r::config()

# Check status of changes
status()

# Add all changed files
add(path = ".")

# Function for commit message
gen_message <- function(data_update, message) {
  if(data_update %in% TRUE)
  {
    return(paste("Data update", Sys.Date()))
  }
  if(data_update %in% FALSE)
  {
    return(paste(message))
  }
}

# Commit message
# Two arguments: data_update and message
# If the commit is a data update, declare gen_message(data_update = TRUE)
# If the commit is something else, declare gen_message(data_update = FALSE, message = "[INPUT YOUR MESSAGE]")
commit_message <- gen_message(data_update = TRUE)

# Commit (CHANGE MESSAGE)
commit(repo = ".", commit_message)

# Push to private repository
system2("git", args = c("-C ./", "push"))


