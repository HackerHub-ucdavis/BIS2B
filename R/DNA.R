
#' expection of one genotype frequence under HWE
#' @param allele_freq list frequence of each allele
#' @param geno str genotype
#' @return double exception of genotype frequence
#' @examples
#' afreq <- allele_frequency(geno_freq)
#' expect_genotype_frequency(afreq)('Aa')
#' @export
expect_genotype_frequency <- function(allele_freq) {
    function(geno) {
        alleles <- BIS2B::str2chars(geno)
        e <- prod(sapply(alleles, function(a) allele_freq[[a]]))
        ifelse(length(unique(alleles)) != 1, 2 * e, e)
    }
}

#' calculate allele frequency based on genotype frequency
#' @param geno_freq list of genotype frequency
#' @return list of allele frequency
#' @examples
#' allele_frequency(list(AA = 0.01, Aa = 0.18, aa = 0.81))
#' @export
allele_frequency <- function(geno_freq) {
    allele_freq <- new.env(hash = T, parent = emptyenv())
    genos <- Map(BIS2B::str2chars, names(geno_freq))
    all_allele <- unique(Reduce(`c`, genos))
    for (allele in all_allele) {
        allele_freq[[allele]] <- 0
    }
    for (geno in names(geno_freq)) {
        alleles <- genos[geno][[1]]
        n_allele <- length(alleles)

        for (allele in alleles) {
            allele_freq[[allele]] <-
                allele_freq[[allele]] + geno_freq[[geno]] / n_allele
        }
    }

    as.list(allele_freq)
}


#' check for Hardy-Weinberg Equilibrium for a list of genotypes
#' @param genotype_freq vector in form of AA, Aa, aa
#' @param epsilon threshold for difference
#' @examples
#' is_HWE(list(AA = 0.01, Aa = 0.18, aa =  0.81))
#' is_HWE(list(AA = 0.25, Aa = 0.5,  aa = 0.25))
#' is_HWE(list(AA = 0.36, Aa = 0.6,  aa = 0.04))
#' is_HWE(list(AA = 0.09, Aa = 0.42, aa =  0.49))
#' is_HWE(list(AA = 0.64, Aa = 0.2,  aa = 0.16))
#' @return bool whether it is at HW equilibrium
#' @export
is_HWE <- function(genotype_freq, epsilon = 1e-5) {
    if (abs(sum(unlist(genotype_freq)) - 1) > epsilon) {
        return(FALSE)
    }

    allele_freq <- allele_frequency(genotype_freq)
    excepted_freq <- lapply(
        names(genotype_freq), expect_genotype_frequency(allele_freq)
    )
    diff <- abs(unlist(genotype_freq) - unlist(excepted_freq))
    all(diff <= epsilon)
}
