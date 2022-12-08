# who: phil henrickson
# what: save datasets for training materials

# packages

library(tidymodels)
library(data.table)
tidymodels_prefer()

# ames --------------------------------------------------------------------


# load version of ames housing data used in tidymodeling with R tutorial
data(ames)

# split into train and test sets, stratifying on logged sale price
ames <- ames %>%
        mutate(log_Sale_Price = log10(Sale_Price))

# split
set.seed(1999)
ames_split <- initial_split(ames, prop = 0.80, strata = log_Sale_Price)

# undo log (will show later)
ames_train <- training(ames_split) %>%
        select(-log_Sale_Price)

ames_test  <-  testing(ames_split) %>%
        select(-log_Sale_Price)

# write data folder
fwrite(ames_train,
       file = here::here("data", "ames_train.csv"))

fwrite(ames_test,
       file = here::here("data", "ames_test.csv"))



# penguins ----------------------------------------------------------------


# tidied version of palmerpenguins dataset
penguins = palmerpenguins::penguins

# write to data folder
fwrite(penguins,
       file = here::here("data", "penguins.csv"))



