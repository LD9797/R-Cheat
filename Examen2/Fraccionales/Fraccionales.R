
if(!require(FrF2)){install.packages("FrF2")}

# Donde nfactors es numero de factores y nruns es numero de ejecuciones.
dsg <- FrF2(nfactors = 6, nruns = 16)
# 32 -> 8 experimentos
# 1/4 2^k

summary(dsg) # Diseño con la resolución más alta posible.

# Se van a requerir dos funciones generadoras.

# $generators
# [1] D=AB E=AC

# Numero de ejecuciones.
# Como los factores están solapados.

# Otra manera

# Resolución -> 1/2 2^k | Cuantos factores se consideran todas sus combinaciones.
# Donde nfactors es numero de factores y resolution es numero de factores principales.
dsg <- FrF2(nfactors = 6, resolution = 16)
summary(dsg)

# R solo muestra las interacciones entre factores principales e interacciones de dos niveles
# debido a la propiedad de escasez de efectos. Para interacciones niveles superiores no se analizan.