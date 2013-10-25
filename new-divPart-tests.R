# Testing divePartNew with divPartOld

# load all original function
source("R/diveRsity-code.R")
# load new divPart
source("divPart-dev.R")

# test new function

# without parallel 
system.time({
newResnp <- divPartNew(infile = "tst-files/KK_test.gen",
                     outfile = "tst-files/New-no-para",
                     parallel = FALSE, bs_locus = TRUE,
                     bs_pairwise = TRUE, pairwise = TRUE,
                     WC_Fst = TRUE, bootstraps = 10, plot = TRUE)
})
# with parallel
system.time({
newResp <- divPartNew(infile = "tst-files/KK_test.gen",
                     outfile = "tst-files/New-para",
                     parallel = FALSE, bs_locus = FALSE,
                     bs_pairwise = TRUE, pairwise = TRUE,
                     WC_Fst = TRUE, bootstraps = 5, plot = TRUE)
})

