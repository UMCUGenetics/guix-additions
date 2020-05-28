(define-module (umcu packages r-survminer)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system r)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages statistics))


(define-public r-km-ci
  (package
    (name "r-km-ci")
    (version "0.5-2")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "km.ci" version))
        (sha256
          (base32
            "1l6kw8jppaa1802yc5pbfwwgac56nhwc9p076ivylhms4w7cdf8v"))))
    (properties `((upstream-name . "km.ci")))
    (build-system r-build-system)
    (propagated-inputs `(("r-survival" ,r-survival)))
    (home-page
      "https://cran.r-project.org/web/packages/km.ci")
    (synopsis
      "Confidence intervals for the Kaplan-Meier estimator")
    (description
      "Computes various confidence intervals for the Kaplan-Meier estimator, namely: Petos CI, Rothman CI, CI's based on Greenwoods variance, Thomas and Grunkemeier CI and the simultaneous confidence bands by Nair and Hall and Wellner.")
    (license license:gpl2+)))

(define-public r-kmsurv
  (package
    (name "r-kmsurv")
    (version "0.1-5")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "KMsurv" version))
        (sha256
          (base32
            "0hi5vvk584rl70gbrr75w9hc775xmbxnaig0dd6hlpi4071pnqjm"))))
    (properties `((upstream-name . "KMsurv")))
    (build-system r-build-system)
    (home-page
      "https://cran.r-project.org/web/packages/KMsurv")
    (synopsis
      "Data sets from Klein and Moeschberger (1997), Survival Analysis")
    (description
      "Data sets and functions for Klein and Moeschberger (1997), \"Survival Analysis, Techniques for Censored and Truncated Data\", Springer.")
    (license license:gpl3+)))

(define-public r-survmisc
  (package
    (name "r-survmisc")
    (version "0.5.5")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "survMisc" version))
        (sha256
          (base32
            "00nvvl8gz4477ab24rd0xvfksm8msv8h021b9ld5c9cizc41n2bm"))))
    (properties `((upstream-name . "survMisc")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-data-table" ,r-data-table)
        ("r-ggplot2" ,r-ggplot2)
        ("r-gridextra" ,r-gridextra)
        ("r-km-ci" ,r-km-ci)
        ("r-kmsurv" ,r-kmsurv)
        ("r-knitr" ,r-knitr)
        ("r-survival" ,r-survival)
        ("r-xtable" ,r-xtable)
        ("r-zoo" ,r-zoo)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page
      "https://cran.r-project.org/web/packages/survMisc")
    (synopsis
      "Miscellaneous Functions for Survival Data")
    (description
      "This package provides a collection of functions to help in the analysis of right-censored survival data.  These extend the methods available in package:survival.")
    (license license:gpl2)))

(define-public r-exactranktests
  (package
    (name "r-exactranktests")
    (version "0.8-31")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "exactRankTests" version))
        (sha256
          (base32
            "1154dkcid3njhamdp87qs9bnx7l8bdqkcjsds9q9f2xmizs9x8gw"))))
    (properties
      `((upstream-name . "exactRankTests")))
    (build-system r-build-system)
    (home-page
      "https://cran.r-project.org/web/packages/exactRankTests")
    (synopsis
      "Exact Distributions for Rank and Permutation Tests")
    (description
      "Computes exact conditional p-values and quantiles using an implementation of the Shift-Algorithm by Streitberg & Roehmel.")
    (license license:gpl2+)))

(define-public r-maxstat
  (package
    (name "r-maxstat")
    (version "0.7-25")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "maxstat" version))
        (sha256
          (base32
            "114z1rwxwvk05ijjhdppzm148n1h192fp0w12ky10zkrhf6kphbg"))))
    (properties `((upstream-name . "maxstat")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-exactranktests" ,r-exactranktests)
        ("r-mvtnorm" ,r-mvtnorm)))
    (home-page
      "https://cran.r-project.org/web/packages/maxstat")
    (synopsis "Maximally Selected Rank Statistics")
    (description
      "Maximally selected rank statistics with several p-value approximations.")
    (license license:gpl2+)))

(define-public r-survminer
  (package
    (name "r-survminer")
    (version "0.4.7")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "survminer" version))
        (sha256
          (base32
            "1pdj3gs4aii8gn8wf4smbwmjymbzwkjwr3kxf90dxyy6i66mqq3v"))))
    (properties `((upstream-name . "survminer")))
    (build-system r-build-system)
    (propagated-inputs
      `(("r-broom" ,r-broom)
        ("r-dplyr" ,r-dplyr)
        ("r-ggplot2" ,r-ggplot2)
        ("r-ggpubr" ,r-ggpubr)
        ("r-gridextra" ,r-gridextra)
        ("r-magrittr" ,r-magrittr)
        ("r-maxstat" ,r-maxstat)
        ("r-purrr" ,r-purrr)
        ("r-rlang" ,r-rlang)
        ("r-scales" ,r-scales)
        ("r-survival" ,r-survival)
        ("r-survmisc" ,r-survmisc)
        ("r-tibble" ,r-tibble)
        ("r-tidyr" ,r-tidyr)))
    (native-inputs `(("r-knitr" ,r-knitr)))
    (home-page
      "http://www.sthda.com/english/rpkgs/survminer/")
    (synopsis
      "Drawing Survival Curves using 'ggplot2'")
    (description
      "Contains the function 'ggsurvplot()' for drawing easily beautiful and 'ready-to-publish' survival curves with the 'number at risk' table and 'censoring count plot'.  Other functions are also available to plot adjusted curves for `Cox` model and to visually examine 'Cox' model assumptions.")
    (license license:gpl2)))

