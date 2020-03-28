train_dockets <- read.csv("raw_data/train_dockets.csv")
train_other_motions <- read.csv("raw_data/train_other_motions.csv")
train_terminating <- read.csv("raw_data/train_terminating_motions.csv")

test_dockets <- read.csv("raw_data/test_dockets.csv")
test_other_motions <- read.csv("raw_data/test_other_motions.csv")
test_terminating_motions <- read.csv("raw_data/test_terminating_motions.csv")

plot(train_dockets)
