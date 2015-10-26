require(xtable)

# Nu
data(Nu.doubleState)
data(Nu.doubleStateSwitch)
data(Nu.tripleStateSwitch)

Nu.params <- data.frame(doubleState = getParameterValues(Nu.doubleState),
                        doubleStateSwitch = getParameterValues(Nu.doubleStateSwitch),
                        tripleStateSwitch = getParameterValues(Nu.tripleStateSwitch))
xtable(Nu.params)
xtable(round(getTransitionMatrix(Nu.doubleStateSwitch), digits = 2), )
xtable(round(getTransitionMatrix(Nu.tripleStateSwitch, nstates = 3), digits = 2))


# Tau
data(Tau.doubleState)
data(Tau.doubleStateSwitch)
data(Tau.tripleStateSwitch)

Tau.params <- data.frame(doubleState = getParameterValues(Tau.doubleState),
                         doubleStateSwitch = getParameterValues(Tau.doubleStateSwitch),
                         tripleStateSwitch = getParameterValues(Tau.tripleStateSwitch))
xtable(Tau.params)
xtable(round(getTransitionMatrix(Tau.doubleStateSwitch), digits = 2))
xtable(round(getTransitionMatrix(Tau.tripleStateSwitch, nstates = 3), digits = 2))


