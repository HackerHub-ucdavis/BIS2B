# Lab 5

#' Calculate the according frequency of phenotypes and allele
#' @param counts total counts of occurance of each phenotype
#' @param pheno_geno_map a map (dataframe) whose keys are phenotype
#'   and values are list of its genes
#' @return list of map, geno frequency, and allele frequency
#' @export
analyz_frequency <- function(counts, pheno_geno_map) {
    total <- sum(counts)
    result <- data.frame(n_pheno = counts)
    result$freq <- counts / total

    all_allele <- unique(unlist(pheno_geno_map))

    # use hash table to calculate frequence for each allele
    allele_freq <- new.env(hash = T, parent = emptyenv())
    for (allele in all_allele) {
        allele_freq[[allele]] <- 0
    }
    for (pheno in colnames(pheno_geno_map)) {
        coor_geno <- pheno_geno_map[[pheno]]
        n_allele <- length(coor_geno)

        for (allele in coor_geno) {
            allele_freq[[allele]] <-
                allele_freq[[allele]] + result[pheno, "freq"] / n_allele
        }
    }

    freqout <- list(
        map = pheno_geno_map,
        geno = result,
        allele = as.list(allele_freq)
    )
    class(freqout) <- c("evolution")
    freqout
}

#' Make prediction aboiut evolution on the current generation
#' @param generation evolution object
#' @returns expected geno frequency table of next generation
#' @export
predict.evolution <- function(generation) {
    allele_freq <- generation$allele
    n <- length(allele_freq)
    f_allele <- lapply(names(allele_freq), function(n) paste0("f(", n, ")"))
    next_gen_freq <- matrix(0,
        nrow = n, ncol = n,
        dimnames = list(f_allele, f_allele)
    )
    for (i in 1:n) {
        for (j in 1:n) {
            next_gen_freq[i, j] <- allele_freq[[i]] * allele_freq[[j]]
        }
    }
    next_gen_freq
}
