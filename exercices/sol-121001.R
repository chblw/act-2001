####  Première approche ####
# a) ----------------------------------------------------------------------

set.seed(2018)
m <- 5

rPP1 <- function(t, lam) {
  Ni <- 0
  while(tail(Ni, 1) < t) {
    Ni <- c(Ni, tail(Ni, 1) + rexp(1, lam))
  }
  Ni[-c(1, length(Ni))]
}

N <- replicate(m, rPP1(10, 1))

# b) ----------------------------------------------------------------------

Z <- numeric(m)

for(i in seq_along(Z)) {
  Ni <- N[[i]]
  Xi <- rlnorm(length(Ni), log(10) - 0.5, 1)
  Z[i] <- sum(exp(-0.03 * Ni) * Xi)
}

Z

# c) ----------------------------------------------------------------------

set.seed(2018)
m <- 1000000
N <- replicate(m, rPP1(10, 1))
Z <- numeric(m)

for(i in seq_along(Z)) {
  Ni <- N[[i]]
  Xi <- rlnorm(length(Ni), log(10) - 0.5, 1)
  Z[i] <- sum(exp(-0.03 * Ni) * Xi)
}

sort(Z)[0.99 * m]

####  Deuxième approche ####
# On définit une fonction générale qui permet de simuler des réalisations d'une police d'asssurance avec n'importe quelle distribution
#   sous-jacente.
simulateLifeClaims <- function(delta, t, lambda, distributionClaims, distributionParameters, seed, numberRepetitions, numberEventsPP1) {
    require(rlang)  ## for the exec function which allows us to call a function uing text
    
    interEventTime <- vector(mode = "list", length = numberRepetitions)
    eventTime <- vector(mode = "list", length = numberRepetitions)
    eventTimeFiltered <- vector(mode = "list", length = numberRepetitions)
    claimCostsIndividuals <- vector(mode = "list", length = numberRepetitions)
    claimCostsAggregated <- numeric(numberRepetitions)
    numberEvents <- numeric(numberRepetitions)
    
    set.seed(seed)
    for (i in 1:numberRepetitions) {
        interEventTime[[i]] <- rexp(numberEventsPP1, lambda)
        eventTime[[i]] <- cumsum(interEventTime[[i]])
        ##  We need to ensure that enough events are simulated and 
        ##      so check the last event occured after t.
        stopifnot(eventTime[[i]][numberEventsPP1] >= t)
        
        ##  We only keep the simulated times relevent to the interval
        eventTimeFiltered[[i]] <- eventTime[[i]][eventTime[[i]] <= t]
        numberEvents[[i]] <- length(eventTimeFiltered[[i]])
        
        ##  We simulate individual claim costs (X_k) for the given distribution 
        ##      which are then given.
        ##
        ##  exec function: for example if distributionClaims = "lnorm" then 
        ##      this call is equivalent to rlnorm(numberEvents[[i]], distributionParameters[1], distributionParameters[2])
        claimCostsIndividuals[[i]] <- rlang::exec(
            .fn = paste0("r", distributionClaims),
            numberEvents[[i]],
            !!!distributionParameters
        )
        claimCostsAggregated[i] <- sum(exp(-delta * eventTimeFiltered[[i]]) * claimCostsIndividuals[[i]])
    }
    list(
        "Aggregate Costs" = claimCostsAggregated,
        "Time of Events" = eventTimeFiltered,
        "Individual Cost of Events" = claimCostsIndividuals,
        "Number of Events" = numberEvents
    )
}
# Pour le numéro 2 : 
simulatedClaimsData <- simulateLifeClaims(
    delta = 0.03, t = 10, lambda = 1,
    distributionClaims = "lnorm", distributionParameters = list(log(10) - 0.5, 1), 
    seed = 2018, numberRepetitions = 1E4, numberEventsPP1 = 1E2
)

# Visualisation des montants aggrégés simulés :
library(ggplot2)
ggplot(data = data.frame(cost = simulatedClaimsData$`Aggregate Costs`, simulation = seq_len(1E4)), aes(cost)) + 
    geom_histogram(fill = "gray83", bins = 60) + 
    theme_classic() + 
    labs(
        y = "Fréquence",
        x = "Coût"
    )

## graphique plus intense qui permet de mieux visualiser :
simulatedAggregateCosts <- simulatedClaimsData$`Aggregate Costs`
simulatedAggregateCostsPlot <- ggplot(data = data.frame(cost = simulatedAggregateCosts), aes(cost)) + 
    geom_histogram(
        fill = "gray83",
        bins = 60
    ) + 
    theme_classic() + 
    labs(
        y = "Fréquence",
        x = "Coût"
    )

##  Get the frequencies to then identify the maximum one for drawing the rectangles sbelow
countFrequenciesHistogram <- ggplot_build(simulatedAggregateCostsPlot)$data[[1]]$count
quantiles <- quantile(simulatedAggregateCosts)

##  Plot the data with rectangles for the quantiles and a line of the mean
simulatedAggregateCostsPlot +
    geom_rect(
        aes(
            xmin = quantiles[1], xmax = quantiles[2], 
            ymin = 0, ymax = max(countFrequenciesHistogram)
        ),
        alpha = 0, fill = "white", colour = "navy", text = "test"
    )  +
    geom_rect(
        aes(
            xmin = quantiles[2], xmax = quantiles[3],
            ymin = 0, ymax = max(countFrequenciesHistogram)
        ),
        alpha = 0, fill = "white", colour = "navy"
    ) +
    geom_rect(
        aes(
            xmin = quantiles[3], xmax = quantiles[4],
            ymin = 0, ymax = max(countFrequenciesHistogram)
        ),
        alpha = 0, fill = "white", colour = "navy"
    ) +
    geom_rect(
        aes(
            xmin = quantiles[4], xmax = quantiles[5],
            ymin = 0, ymax = max(countFrequenciesHistogram)
        ),
        alpha = 0, fill = "white", colour = "navy"
    ) +
    geom_vline(
        aes(xintercept = mean(simulatedAggregateCosts)),
        linetype = "dashed", colour = "gray3"
    ) +
    annotate(
        geom = "text", 
        x = mean(simulatedAggregateCosts) + 10, y = 25, 
        label = 'paste("", bar(paste("Z")))', parse = TRUE # parse le LaTeX
    )  +
    geom_text(
        data = data.frame(perc = (quantiles[-5] + quantiles[-1])/2, med = 300, text = paste0("Q", 1:4)),
        mapping = aes(x = perc, y = med, label = text)
    )
