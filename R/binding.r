
#' Response at equilibrium
#'
#' Returns the response value at equilibrium from concentration, Rmax and KD.
#' @param conc Analyte concentration.
#' @param rmax Maximum response.
#' @param kd Equilibrium dissociation constant.
#' @export
#' @examples
#' req(6e-7,1.2,6e-7)
req <- function(conc, rmax, kd) {

  if (conc < 0) warning("Invalid concentration")
  if (rmax < 0) warning("Invalid Rmax")

  (conc * rmax) / (conc + kd)
}

#' Time to Equilibrium
#'
#' Returns the time taken to reach 95\% equilibrium.
#' @param conc Analyte concentration.
#' @param kon Kon binding constant.
#' @param koff Koff binding constant.
#' @param theta Default 0.95.
#' @export
#' @examples
#' tteq(6e-7,20000,0.01)
tteq <- function(conc, kon, koff, theta = 0.95) {
  (-1 * log(1 - theta)) / (kon * conc + koff)
}

association <- function(t, conc, kon, koff, rmax) {
  req(conc, rmax, (koff / kon)) * (1 - exp(-1 * (kon * conc + koff) * t))
}

dissociation <- function(r0, koff, t) {
  r0 * exp(-1 * koff * t)
}

#' Generate a 1:1 Binding Curve
#'
#' Returns a response value for given parameters at time, t.
#' @param t Time.
#' @param t0 Time of dissociation.
#' @param conc Analyte concentration.
#' @param kon Kon binding constant.
#' @param koff Koff binding constant.
#' @param rmax Maximum response, Rmax.
#' @param drift Optional. Parameter to add a linear baseline drift.
#' @param offset Optional. Applies a global offset to the response value.
#' @param doffset Optional. Applies an offset at the start of dissociation.
#' @keywords binding1to1
#' @export
#' @examples
#' time <- seq(1,2000)
#' curve <- binding1to1(time,1000,6e-9,1000,0.01,0.6)
#' plot(curve)
binding1to1 <- function(t, t0, conc, kon, koff, rmax, drift = 0, offset = 0,
  doffset = 0) {

  if (drift > 0) warning("Drift parameter set")
  if (any(t < 0)) stop("Negative value for t")
  if (any(t0 <= 0)) stop("Invalid value for t0")

  ifelse(t < t0,
    association(t, conc, kon, koff, rmax) + (drift * t) + offset,
    dissociation(association(t0, conc, kon, koff, rmax), koff, t - t0)  +
    (drift * t) + offset + doffset
  )
}



#' Generate a 2:1 Binding Curve
#'
#' Returns a response value for given parameters at time, t.
#' @param t Time.
#' @param t0 Time of dissociation.
#' @param conc Analyte concentration.
#' @param kon1 Kon binding constant for first component.
#' @param koff1 Koff binding constant for first component.
#' @param rmax1 Maximum response, Rmax, for first component.
#' @param kon2 Kon binding constant for second component.
#' @param koff2 Koff binding constant for second component.
#' @param rmax2 Maximum response, Rmax, for second component.
#' @param drift Optional. Parameter to add a linear baseline drift.
#' @param offset Optional. Applies a global offset to the response value.
#' @param doffset Optional. Applies an offset at the start of dissociation.
#' @keywords binding2to1
#' @export
#' @examples
#' time <- seq(1,2000)
#' curve <- binding2to1(time,1000,900e-9,10000,0.01,0.4,2000,0.0003,0.5)
#' plot(curve)
binding2to1 <- function(t, t0, conc, kon1, koff1, rmax1, kon2, koff2, rmax2,
  drift = 0, offset = 0, doffset = 0) {

  if (drift > 0) warning("Drift parameter set")
  if (any(t < 0)) stop("Cannot have negative value for t")
  if (any(t0 <= 0)) stop("Invalid value for t0")


  ifelse(t < t0,
    association(t, conc, kon1, koff1, rmax1) +
    association(t, conc, kon2, koff2, rmax2) +
    (drift * t) + offset,
    dissociation(association(t0, conc, kon1, koff1, rmax1), koff1, t - t0) +
    dissociation(association(t0, conc, kon2, koff2, rmax2), koff2, t - t0) +
    (drift * t) + offset + doffset
  )
}
