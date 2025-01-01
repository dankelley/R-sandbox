# Bubble sort rankings
rankr <- function(names, ..., debug = FALSE) {
    msg <- function(...) if (debug) cat(..., sep = "")
    nnames <- length(names)
    nunames <- length(unique(names))
    if (nnames != nunames) {
        stop("names are not unique")
    }
    dots <- list(...)
    if (length(dots) == 0) {
        stop("must supply rankings")
    }
    ms <- sapply(dots, \(item) length(item))
    if (any(ms != ms[1])) {
        stop("unequal ranking lengths ", paste(ms, collapse = " "))
    }
    if (ms[1] != length(names)) {
        stop("name length ", names, " but ranking length ", ms[1])
    }
    rankings <- data.frame(...)
    m <- ncol(rankings)
    v <- data.frame(names, rankings)
    colnames(v) <- c("Name", paste("R", 1:m, sep = ""))
    vorig <- v
    m <- ncol(rankings)
    if (m %% 2 != 1) {
        stop("must have an odd number of rankers")
    }
    criterion <- m / 2
    if (debug) {
        cat("Original ranking matrix\n")
        print(vorig)
    }
    nexch <- 0
    n <- length(names)
    for (era in seq(1, n - 1)) {
        for (i in seq(1, n - 1)) {
            w <- sapply(seq(2, m + 1), \(j) v[i, j] > v[i + 1, j]) |> sum()
            if (w > criterion) {
                msg(
                    "  era=", era, " i=", i, " w=", w, " (> ", criterion,
                    " so exch ", i, " & ", i + 1, ")\n"
                )
                tmp <- v[i, ]
                v[i, ] <- v[i + 1, ]
                v[i + 1, ] <- tmp
                nexch <- nexch + 1
                if (debug) print(v)
            } else {
                msg(
                    "  era=", era, " i=", i, " w=", w, " (<=", criterion,
                    " so no exch)\n"
                )
            }
        }
    }
    # Now, v is in ranked order. But we want to return the original
    # version of v, with an additional column.
    vnew <- data.frame(vorig, Rank = as.vector(sapply(
        names,
        \(name) which(v[, 1] == name)
    )))
    if (debug) {
        cat("Final ranking matrix, after", nexch, "row exchanges\n")
        print(vnew)
    }
    vnew
}

# Demonstration
dataText <- "
Name R1 R2 R3
   A  3  2  4
   B  5  4  5
   C  1  3  1
   D  4  1  3
   E  2  5  2
"
d <- read.table(text = dataText, header = TRUE)
R <- rankr(d$Name, d$R1, d$R2, d$R3)
# 'score' and 'rankNew' are a simpler way of working but...
score <- R$R1 + R$R2 + R$R3
R$RankNew <- order(order(score, decreasing = FALSE))
R$SumOfRanks <- score
R[order(R$Rank), ]
cat("Notice that RankNew does not equal Rank, and that some scores are equal.\n")
cat("Therefore, Rank is preferred to RankNew, and the latter is only shown\n")
cat("for demonstration purposed.\n")
