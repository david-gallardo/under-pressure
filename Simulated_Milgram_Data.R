require(ggplot2)
GenBetaDist <- function(n, a, b, min = 1, max = 29)
{
  # n <- number of observations
  
  # a>b <- right skewed distribution
  # a<b <- left skewed distribution
  # a=b <- normal distribution
  
  # generate beta observations and linearly transform
  x <- rbeta(n, a, b)
  y <- (max - min) * x + min
  
  return(as.integer(y))
  
}

# We generate the random samples of all personality traits according to our hypotheses

n = 100  # set the sample size

Trials <- as.data.frame(seq(1:29))
names(Trials) <- "Trial"

N <-
  GenBetaDist(n, 6, 1)                    # More important at the end
E <-
  as.integer(runif(n, min = 1, max = 29)) # Effect uniformly distributed
O <-
  as.integer(runif(n, min = 1, max = 29)) # Effect uniformly distributed
A <-
  GenBetaDist(n, 2, 4)                    # More important at the beginning
C <-
  GenBetaDist(n, 6, 1)                    # More important at the end

Coercion <-
  GenBetaDist(n, 8, 1)             # Much more important at the end
Proximity <-
  GenBetaDist(n, 1, 6)            # More important at the end

# Compute the frequencies for each trial

NFreqs <- as.data.frame(table(N))
names(NFreqs) <- c("Trial", "N")

EFreqs <- as.data.frame(table(E))
names(EFreqs) <- c("Trial", "E")

OFreqs <- as.data.frame(table(O))
names(OFreqs) <- c("Trial", "O")

AFreqs <- as.data.frame(table(A))
names(AFreqs) <- c("Trial", "A")

CFreqs <- as.data.frame(table(C))
names(CFreqs) <- c("Trial", "C")

CoercionFreqs <- as.data.frame(table(Coercion))
names(CoercionFreqs) <- c("Trial", "Coercion")

ProximityFreqs <- as.data.frame(table(Proximity))
names(ProximityFreqs) <- c("Trial", "Proximity")

# Merge all data in a single data.frame

all.data <- merge.data.frame(Trials, NFreqs, by = "Trial", all = T)
all.data <- merge.data.frame(all.data, EFreqs, by = "Trial", all = T)
all.data <- merge.data.frame(all.data, OFreqs, by = "Trial", all = T)
all.data <- merge.data.frame(all.data, AFreqs, by = "Trial", all = T)
all.data <- merge.data.frame(all.data, CFreqs, by = "Trial", all = T)
all.data <-
  merge.data.frame(all.data, CoercionFreqs, by = "Trial", all = T)
all.data <-
  merge.data.frame(all.data, ProximityFreqs, by = "Trial", all = T)

all.data[is.na(all.data)] <- 0

# Write a CSV file for replication purposes
write.csv2(all.data, "data_simulation_milgram.csv")

# Print plot
p = ggplot() +
  geom_line(
    data = all.data,
    aes(x = Trial, y = all.data$N),
    color = "black",
    linetype = "dashed"
  ) +
  geom_line(
    data = all.data,
    aes(x = Trial, y = all.data$C),
    color = "black",
    linetype = "solid"
  ) +
  geom_line(
    data = all.data,
    aes(x = Trial, y = all.data$A),
    color = "black",
    linetype = "twodash"
  ) +
  geom_line(
    data = all.data,
    aes(x = Trial, y = all.data$Coercion),
    color = "black",
    linetype = "dotted"
  ) +
  scale_x_continuous("Trials",
                     labels = as.character(all.data$Trial),
                     breaks = all.data$Trial) +
  xlab('Trial') +
  ylab('Relative importance')
p

# Remove everything from the environment
rm(list = ls())