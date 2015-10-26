require(xtable)

# Lamprey
data(Lamprey.doubleState)
data(Lamprey.doubleStateSwitch)
data(Lamprey.tripleStateSwitch)

Lamprey.params <- data.frame(doubleState = getParameterValues(Lamprey.doubleState),
                             doubleStateSwitch = getParameterValues(Lamprey.doubleStateSwitch),
                             tripleStateSwitch = getParameterValues(Lamprey.tripleStateSwitch))
xtable(Lamprey.params)
xtable(round(getTransitionMatrix(Lamprey.doubleStateSwitch), digits = 2), )
xtable(round(getTransitionMatrix(Lamprey.tripleStateSwitch, nstates = 3), digits = 2))


# Wolf
data(Wolf.doubleState)
data(Wolf.doubleStateSwitch)
data(Wolf.tripleStateSwitch)

Wolf.params <- data.frame(doubleState = getParameterValues(Wolf.doubleState),
                             doubleStateSwitch = getParameterValues(Wolf.doubleStateSwitch),
                             tripleStateSwitch = getParameterValues(Wolf.tripleStateSwitch))
xtable(Wolf.params)
xtable(round(getTransitionMatrix(Wolf.doubleStateSwitch), digits = 2))
xtable(round(getTransitionMatrix(Wolf.tripleStateSwitch, nstates = 3), digits = 2))


# WFR
data(WFR.doubleState)
data(WFR.doubleStateSwitch)
data(WFR.tripleStateSwitch)

WFR.params <- data.frame(doubleState = getParameterValues(WFR.doubleState),
                             doubleStateSwitch = getParameterValues(WFR.doubleStateSwitch),
                             tripleStateSwitch = getParameterValues(WFR.tripleStateSwitch))
xtable(WFR.params)
xtable(round(getTransitionMatrix(WFR.doubleStateSwitch), digits = 2))
xtable(round(getTransitionMatrix(WFR.tripleStateSwitch, nstates = 3), digits = 2))

xtable(cbind(Lamprey.params, Wolf.params, WFR.params))
