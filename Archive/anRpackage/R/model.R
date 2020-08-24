model <-
function(times, init, parms) {
  with(as.list(c(init, parms)), {
    beta <- (1-reductionList[times])*R0*kappa/population
    dS <- -beta * S * I #susceptible
    dE <- beta * S * I -  E * kappa #exposed but asymptomatic
    dI <- E * kappa - I * kappa2 #infectious, but mild severity
    dH <- eta * I  * kappa2 - tau * H #hospitalized
    dC <- eta2 * tau * H  - tau2 * C #critical care
    dR <- (1 - eta) * I * kappa2 + (1 - eta2) * tau * H + (1 - epsilon) * tau2 * C #recovered
    dD <- epsilon * tau2 * C #dead
    hosp <- eta * I  * kappa2 # incident hospitalizations
    crits <- eta2 * tau * H # incident ICUs
    list(c(dS, dE, dI, dH, dC, dR, dD, hosp, crits))
  })
}
