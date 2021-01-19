#' rcc2ColShade
#' @description internal function.
#' @author Fredrik Sandin, RCC Mellansverige
#' @keywords internal
#' @export
rcc2ColShade <-
  function(
    col = "#000000",
    factor = seq(0, 2, 0.1)
  ) {
    # Check
    if (any(factor < 0 | factor > 2)) {
      stop("Invalid values for factor, only values between 0 and 2 allowed")
    }

    R <- rep(strtoi(substr(col[1], 2, 3), 16), length(factor))
    G <- rep(strtoi(substr(col[1], 4, 5), 16), length(factor))
    B <- rep(strtoi(substr(col[1], 6, 7), 16), length(factor))

    R_add <- rep(0, length(factor))
    G_add <- rep(0, length(factor))
    B_add <- rep(0, length(factor))

    factorLessThanOne <- factor < 1
    R_add[factorLessThanOne] <- -((R - 0) * (1 - factor))[factorLessThanOne]; R_add[!factorLessThanOne] <- ((255 - R) * (factor - 1))[!factorLessThanOne]
    G_add[factorLessThanOne] <- -((G - 0) * (1 - factor))[factorLessThanOne]; G_add[!factorLessThanOne] <- ((255 - G) * (factor - 1))[!factorLessThanOne]
    B_add[factorLessThanOne] <- -((B - 0) * (1 - factor))[factorLessThanOne]; B_add[!factorLessThanOne] <- ((255 - B) * (factor - 1))[!factorLessThanOne]

    RR <- as.character(as.hexmode(round(R + R_add)))
    GG <- as.character(as.hexmode(round(G + G_add)))
    BB <- as.character(as.hexmode(round(B + B_add)))

    RR_lengtone <- nchar(RR) == 1
    GG_lengtone <- nchar(GG) == 1
    BB_lengtone <- nchar(BB) == 1
    RR[RR_lengtone] <- paste0("0", RR[RR_lengtone])
    GG[GG_lengtone] <- paste0("0", GG[GG_lengtone])
    BB[BB_lengtone] <- paste0("0", BB[BB_lengtone])

    return(
      paste0(
        "#",
        RR,
        GG,
        BB
      )
    )
  }
