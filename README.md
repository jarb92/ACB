# ACB

#############################################
# DEMANDA DINÁMICA + ELASTICIDADES + INGRESOS COMERCIALES
# CON SHOCK DE RENTA EN 2032 (-1%)
#############################################

# Cargar librerías necesarias
library(ggplot2)
library(tidyr)
library(tidyverse)
library(dplyr)



# ------------------------------
# PARÁMETROS DEL PROYECTO
# ------------------------------
anio_inicio <- 2026

horizonte <- 25
anios <- anio_inicio:(anio_inicio + horizonte - 1)

tasa_descuento <- 0.03
anio_inicio_operacion <- 2029

poblacion_total_inicial <- 94862
proporcion_activa <- 0.65
tasa_crecimiento_poblacion <- 0.005
tasa_crecimiento_uso <- 0.001

porcentaje_uso <- list(
  Conservador = 0.05,
  Base = 0.10,
  Ambicioso = 0.15
)

dias_operativos <- 300

# ------------------------------
# ELASTICIDADES Y VARIABLES ECONÓMICAS
# ------------------------------
renta_base <- 39555
precio_tp_base <- 1.50
precio_auto_base <- 0.50

## Supuestos de elasticidades
e_renta <- 0.1 
e_tp <- 0.25
e_auto <- -0.35

#Supuestos de tasas de crecimiento

crecimiento_renta <- 0.01 # Suponsiciòn de aumento de renta del 1% anual
crecimiento_precio_tp <- 0.015 #SUposiciòn de un aumento de precio del transporte del 1.5%
crecimiento_precio_auto <- 0.02  #SUposiciòn de un aumento de precio del auto personal del 2%

# ------------------------------
# INGRESOS COMERCIALES
# ------------------------------
ingresos_comerciales_iniciales <- 250000
tasa_crecimiento_ingresos <- 0.02

# ------------------------------
# INICIALIZACIÓN
# ------------------------------
poblacion_total_anual <- numeric(length(anios))
poblacion_activa_anual <- numeric(length(anios))
pasajeros_anuales <- data.frame(
  Anio = anios,
  Conservador = numeric(length(anios)),
  Base = numeric(length(anios)),
  Ambicioso = numeric(length(anios))
)

renta_anual <- numeric(length(anios))
precio_tp_anual <- numeric(length(anios))
precio_auto_anual <- numeric(length(anios))
factor_ajuste_demanda <- numeric(length(anios))
ingresos_comerciales <- numeric(length(anios))

# ------------------------------
# CÁLCULOS DEMOGRÁFICOS Y ECONÓMICOS
# ------------------------------
poblacion_total_anual[1] <- poblacion_total_inicial
renta_anual[1] <- renta_base
precio_tp_anual[1] <- precio_tp_base
precio_auto_anual[1] <- precio_auto_base
factor_ajuste_demanda[1] <- 1

for (i in 2:length(anios)) {
  poblacion_total_anual[i] <- poblacion_total_anual[i - 1] * (1 + tasa_crecimiento_poblacion)
  
  # Renta crece normalmente...
  renta_anual[i] <- renta_anual[i - 1] * (1 + crecimiento_renta)
  
  # ...pero desde 2032 cae un 1%
  if (anios[i] >= 2026) {
    renta_anual[i] <- renta_anual[i]*1 # Efecto de movimientos en renta
  }
  
  precio_tp_anual[i] <- precio_tp_anual[i - 1] * (1 + crecimiento_precio_tp)
  precio_auto_anual[i] <- precio_auto_anual[i - 1] * (1 + crecimiento_precio_auto)
}

poblacion_activa_anual <- poblacion_total_anual * proporcion_activa

# ------------------------------
# FACTOR DE AJUSTE DE DEMANDA
# ------------------------------
for (i in 1:length(anios)) {
  factor_ajuste_demanda[i] <- 
    (renta_anual[i] / renta_base)^e_renta *
    (precio_tp_base / precio_tp_anual[i])^e_tp *   # <- invertido aquí
    (precio_auto_anual[i] / precio_auto_base)^e_auto
}

# ------------------------------
# PASAJEROS POR AÑO Y ESCENARIO
# ------------------------------
for (i in 1:length(anios)) {
  anio <- anios[i]
  multiplicador_uso <- (1 + tasa_crecimiento_uso)^(i - 1)
  
  for (escenario in names(porcentaje_uso)) {
    if (anio < anio_inicio_operacion) {
      pasajeros_anuales[[escenario]][i] <- 0
    } else {
      tasa_uso_dinamica <- porcentaje_uso[[escenario]] * multiplicador_uso
      usuarios_diarios <- poblacion_activa_anual[i] * tasa_uso_dinamica
      pasajeros_anuales[[escenario]][i] <- usuarios_diarios * dias_operativos
    }
  }
}

# ------------------------------
# AJUSTE POR ELASTICIDADES
# ------------------------------
pasajeros_ajustados <- pasajeros_anuales
for (escenario in c("Conservador", "Base", "Ambicioso")) {
  pasajeros_ajustados[[escenario]] <- pasajeros_anuales[[escenario]] * factor_ajuste_demanda
}

# ------------------------------
# INGRESOS COMERCIALES
# ------------------------------
for (i in 1:length(anios)) {
  if (anios[i] < anio_inicio_operacion) {
    ingresos_comerciales[i] <- 0
  } else {
    ingresos_comerciales[i] <- ingresos_comerciales_iniciales * ((1 + tasa_crecimiento_ingresos)^(i - which(anios == anio_inicio_operacion)))
  }
}

# ------------------------------
# RESULTADO FINAL
# ------------------------------
resultados_demanda <- data.frame(
  Anio = anios,
  Pasajeros_Conservador = pasajeros_ajustados$Conservador/1e6,
  Pasajeros_Base = pasajeros_ajustados$Base/1e6,
  Pasajeros_Ambicioso = pasajeros_ajustados$Ambicioso/1e6,
  Ingresos_Comerciales = ingresos_comerciales/1e6
)

# ------------------------------
# GRAFICAR RESULTADO
# ------------------------------
flujos_largos <- pivot_longer(resultados_demanda,
                              cols = starts_with("Pasajeros"),
                              names_to = "Escenario",
                              values_to = "Pasajeros")

ggplot(flujos_largos, aes(x = Anio, y = Pasajeros, color = Escenario, linetype = Escenario)) +
  geom_line(size = 1) +
  scale_y_continuous(breaks = seq(0, max(flujos_largos$Pasajeros, na.rm = TRUE), by = 0.5)) +
  labs(
    x = "Año",
    y = "Millones de Pasajeros por Año",
    color = "Escenario",
    linetype = "Escenario"
  ) +
  theme_minimal()




#############################################
# MONETIZACIÓN DE BENEFICIOS INTEGRADOS
# Ahorro de tiempo + Emisiones evitadas + Ahorro energético
#############################################

# --------------------------
# PARÁMETROS GENERALES
# --------------------------
anio_inicio <- 2026
anio_inicio_operacion <- 2029
horizonte <- 25
anios <- anio_inicio:(anio_inicio + horizonte - 1)
tasa_descuento <- 0.03

# --------------------------
# BENEFICIO 1: AHORRO DE TIEMPO
# --------------------------
tiempo_ahorrado_min <- 6
valor_minuto <- 0.344
beneficio_por_pasajero <- tiempo_ahorrado_min * valor_minuto

# --------------------------
# BENEFICIO 2: EMISIONES EVITADAS
# Supuesto: % de usuarios que provienen del coche
# Factores según PMUS Las Rozas
# --------------------------
porcentaje_modal_auto <- 0.05  # 5% del total de pasajeros provienen del coche

distancia_media_km <- 10
factor_emision_auto_kgkm <- 0.15374  # kg CO2 por km recorrido en coche
valor_social_CO2 <- 100 / 1000       # €/kg CO2 evitado (equivalente a 100 €/tCO2)

# --------------------------
# BENEFICIO 3: AHORRO ENERGÉTICO
# Supuestos PMUS Las Rozas
# --------------------------
consumo_auto_kWh_km <- 0.0485      # kWh/km
consumo_tp_kWh_km <- 0.27731       # kWh/km
ahorro_kWh_km <- consumo_auto_kWh_km - consumo_tp_kWh_km  # negativo (TP consume más)

# Consideramos solo el ahorro por km NO recorridos en coche
precio_energia <- 0.20  # €/kWh

# --------------------------
# FLUJO DE PASAJEROS AJUSTADOS
# --------------------------
# Asegúrate de tener definido `pasajeros_ajustados` del modelo base

# --------------------------
# CÁLCULO DE BENEFICIOS TOTALES POR AÑO Y ESCENARIO
# --------------------------
beneficios <- data.frame(
  Anio = anios,
  Conservador = pasajeros_ajustados$Conservador,
  Base = pasajeros_ajustados$Base,
  Ambicioso = pasajeros_ajustados$Ambicioso
)

for (escenario in c("Conservador", "Base", "Ambicioso")) {
  pasajeros <- beneficios[[escenario]]
  
  # Ahorro de tiempo
  b_tiempo <- pasajeros * beneficio_por_pasajero
  
  # Emisiones evitadas (solo cambio modal)
  pasajeros_modal <- pasajeros * porcentaje_modal_auto
  emisiones_kg <- pasajeros_modal * distancia_media_km * factor_emision_auto_kgkm
  b_emisiones <- emisiones_kg * valor_social_CO2
  
  # Ahorro energético
  energia_kWh <- pasajeros_modal * distancia_media_km * ahorro_kWh_km
  b_energia <- energia_kWh * precio_energia
  
  # Beneficio total anual bruto
  beneficios[[paste0("Beneficio_", escenario)]] <- b_tiempo + b_emisiones + b_energia
}

# --------------------------
# DESCUENTO DE BENEFICIOS
# --------------------------
factor_descuento <- 1 / (1 + tasa_descuento)^(anios - anio_inicio)
for (escenario in c("Conservador", "Base", "Ambicioso")) {
  beneficios[[paste0("Beneficio_", escenario, "_DS")]] <- beneficios[[paste0("Beneficio_", escenario)]] * factor_descuento
}

# --------------------------
# VISUALIZACIÓN DE BENEFICIOS ANUALES
# --------------------------

beneficios_largos <- beneficios %>%
  select(Anio, starts_with("Beneficio_"), -ends_with("_DS")) %>%
  pivot_longer(cols = -Anio, names_to = "Escenario", values_to = "Beneficio")

ggplot(beneficios_largos, aes(x = Anio, y = Beneficio / 1e6, color = Escenario)) +
  geom_line(size = 1) +
  labs(
    x = "Año",
    y = "Millones de €",
    color = "Escenario"
  ) +
  theme_minimal()











#############################################
# COSTES ACTUALIZADOS: OPEX CRECIENTE + RENOVACIÓN TECNOLÓGICA
#############################################

# Parámetros base
anio_inicio <- 2026
horizonte <- 25
anios <- anio_inicio:(anio_inicio + horizonte - 1)
tasa_descuento <- 0.03

# ---------------------------------------
# COSTES DE INVERSIÓN (CAPEX)
# ---------------------------------------
costo_terreno <- 5000000
costo_diseno <- 100000
costo_construccion <- 15000000
costo_vertipuerto <- 10000000
costo_infraestructura_conexa <- 1000000
costo_tecnologia <- 350000 + 100000 + 450000 + 100000 + 100000

capex_base <- costo_terreno + costo_diseno + costo_construccion + costo_vertipuerto +
  costo_infraestructura_conexa + costo_tecnologia

capex_indirectos <- capex_base * 0.05
capex_total <- capex_base + capex_indirectos

# Distribución 2026–2028
capex_anual <- capex_total / 3
costes_inversion <- ifelse(anios %in% 2026:2028, capex_anual, 0)

# ---------------------------------------
# COSTES DE RENOVACIÓN EN 2040 (7.5% del CAPEX)
# ---------------------------------------
anio_renovacion <- 2040
coste_renovacion <- capex_total * 0.075
costes_renovacion <- ifelse(anios == anio_renovacion, coste_renovacion, 0)

# ---------------------------------------
# COSTES OPERATIVOS (OPEX CRECIENTE 2%)
# ---------------------------------------
opex_base <- 750000
opex_anual <- numeric(length(anios))

for (i in 1:length(anios)) {
  if (anios[i] >= 2029) {
    opex_anual[i] <- opex_base * (1.02)^(anios[i] - 2029)
  } else {
    opex_anual[i] <- 0
  }
}

# ---------------------------------------
# FLUJO TOTAL DE COSTES Y DESCUENTO
# ---------------------------------------
costes_totales <- costes_inversion + opex_anual + costes_renovacion
factor_descuento <- 1 / (1 + tasa_descuento)^(anios - anio_inicio)
costes_descontados <- costes_totales * factor_descuento

# ---------------------------------------
# TABLA FINAL DE COSTES
# ---------------------------------------
costes_pgou <- data.frame(
  Anio = anios,
  CAPEX = costes_inversion,
  Renovacion = costes_renovacion,
  OPEX = opex_anual,
  Total_Costes = costes_totales,
  Costes_Descontados = costes_descontados
)

# Visualizar primeros años
head(costes_pgou, 10)



# Convertir a formato largo para graficar
costes_largos <- costes_pgou %>%
  select(Anio, CAPEX, OPEX, Renovacion) %>%
  pivot_longer(cols = c("CAPEX", "OPEX", "Renovacion"), 
               names_to = "Tipo_Coste", 
               values_to = "Valor")

# Crear gráfico de barras apiladas
ggplot(costes_largos, aes(x = Anio, y = Valor / 1e6, fill = Tipo_Coste)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(
    "CAPEX" = "#d95f02", 
    "OPEX" = "#1b9e77", 
    "Renovacion" = "#7570b3"
  )) +
  labs(
    x = "Año",
    y = "Millones de Euros",
    fill = "Tipo de Coste"
  ) +
  theme_minimal()





#######################################################################################

#############################################
# ESCENARIO SIN PROYECTO: COSTES DE OPORTUNIDAD INTEGRADOS
#############################################

# --------------------------
# PARÁMETROS BASE
# --------------------------
anios <- beneficios$Anio
tasa_descuento <- 0.03

# % de población que seguiría usando auto si no hay proyecto
porcentaje_modal_auto_sin_proyecto <- 0.65
porcentaje_modal_auto_con_proyecto <- 0.60
delta_modal_auto <- porcentaje_modal_auto_sin_proyecto - porcentaje_modal_auto_con_proyecto

# Flujo de pasajeros del escenario base
pasajeros_reales <- pasajeros_ajustados$Base

# Descuento
factor_descuento <- 1 / (1 + tasa_descuento)^(anios - anios[1])

# --------------------------
# COMPONENTE 1: TIEMPO PERDIDO
# --------------------------
minutos_no_ahorrados <- 6
valor_minuto <- 0.344  # €/minuto
costo_tiempo <- pasajeros_reales * minutos_no_ahorrados * valor_minuto
costo_tiempo_ds <- costo_tiempo * factor_descuento

# --------------------------
# COMPONENTE 2: EMISIONES QUE NO SE EVITAN
# --------------------------
factor_emision_kgkm <- 0.15374
distancia_km <- 10
valor_CO2_kg <- 0.10

emisiones_excedentes <- pasajeros_reales * delta_modal_auto * distancia_km * factor_emision_kgkm
costo_emisiones <- emisiones_excedentes * valor_CO2_kg
costo_emisiones_ds <- costo_emisiones * factor_descuento

# --------------------------
# COMPONENTE 3: ENERGÍA DESPERDICIADA
# --------------------------
consumo_auto <- 0.0485  # kWh/pasajero-km
consumo_tp <- 0.27731
delta_energia <- consumo_auto - consumo_tp
precio_energia <- 0.20  # €/kWh

energia_extra <- pasajeros_reales * delta_modal_auto * distancia_km * delta_energia
costo_energia <- energia_extra * precio_energia
costo_energia_ds <- costo_energia * factor_descuento

# --------------------------
# COMPONENTE 4: USO ALTERNATIVO DEL TERRENO
# --------------------------
# Supuestos:
# - Superficie estimada: 26,500 m²
# - Valor suelo medio: 909 €/m² → total = 24,098,500 €
# - Tasa de retorno: 5% anual
# → Renta anual alternativa: 1,204,925 €

valor_terreno <- 10000*((676+1500)/2)# 10000 un aprox del intercambiador, 676 y 1500 precio min y max del mmetro cuadrado de terreno
renta_anual_terreno <- valor_terreno * 0.05  # genera 1.44 millones €/año

costo_terreno <- rep(renta_anual_terreno, length(anios))
costo_terreno_ds <- costo_terreno * factor_descuento

# --------------------------
# TOTAL DE COSTES DE OPORTUNIDAD
# --------------------------
coste_oportunidad_total <- costo_tiempo + costo_emisiones + costo_energia - costo_terreno
coste_oportunidad_ds <- costo_tiempo_ds + costo_emisiones_ds + costo_energia_ds - costo_terreno_ds

# --------------------------
# DATAFRAME FINAL
# --------------------------
sin_proyecto <- data.frame(
  Anio = anios,
  Costo_Tiempo = costo_tiempo,
  Costo_Emisiones = costo_emisiones,
  Costo_Energia = costo_energia,
  Costo_Terreno = costo_terreno,
  Coste_Oportunidad_Total = coste_oportunidad_total,
  Coste_Oportunidad_DS = coste_oportunidad_ds
)

# --------------------------
# GRAFICAR RESULTADO
# --------------------------

ggplot(sin_proyecto, aes(x = Anio, y = Coste_Oportunidad_DS / 1e6)) +
  geom_line(color = "firebrick", size = 1.2) +
  labs(
    x = "Año",
    y = "Millones de €",
    color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "none")


#############################################
# COMPARACIÓN: Proyecto Base vs Sin Proyecto
#############################################

# Requiere:
# - flujo_neto (ya calculado previamente)
# - sin_proyecto (costes de oportunidad descontados)

# Crear un data frame combinado
comparacion <- data.frame(
  Anio = sin_proyecto$Anio,
  Flujo_Neto_Proyecto = flujo_neto$Base / 1e6,
  Costo_Oportunidad_Sin_Proyecto = -sin_proyecto$Coste_Oportunidad_DS / 1e6  # Negativo: pérdida evitada
)

# Convertir a formato largo
comparacion_larga <- comparacion %>%
  pivot_longer(cols = -Anio, names_to = "Escenario", values_to = "Valor")


ggplot(comparacion_larga, aes(x = Anio, y = Valor, color = Escenario)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Comparación entre Proyecto y Escenario Sin Proyecto",
    subtitle = "Flujo Neto del Proyecto Base vs Costes de Oportunidad (millones de €)",
    x = "Año",
    y = "Millones de €",
    color = "Escenario"
  ) +
  theme_minimal()













#############################################
# EVALUACIÓN FINANCIERA COMPARADA: CON vs SIN PROYECTO
#############################################

# -------------------------------
# FLUJOS DE COSTES Y BENEFICIOS
# -------------------------------
# Con proyecto:
# beneficios -> columnas: Anio, Beneficio_Conservador_DS, Beneficio_Base_DS, Beneficio_Ambicioso_DS
# costes_pgou -> columna: Costes_Descontados

# Sin proyecto (coste de oportunidad)
# sin_proyecto -> columna: Coste_Oportunidad_DS

# -------------------------------
# VAN del proyecto (como antes)
# -------------------------------
van_conservador <- sum(beneficios$Beneficio_Conservador_DS) - sum(costes_pgou$Costes_Descontados)
van_base <- sum(beneficios$Beneficio_Base_DS) - sum(costes_pgou$Costes_Descontados)
van_ambicioso <- sum(beneficios$Beneficio_Ambicioso_DS) - sum(costes_pgou$Costes_Descontados)

# -------------------------------
# VAN del escenario sin proyecto (como suma de sus costes de oportunidad)
# -------------------------------
van_sin_proyecto <- -sum(sin_proyecto$Coste_Oportunidad_DS)

# -------------------------------
# VAN diferencial (VAN del proyecto - VAN del escenario BAU)
# -------------------------------
van_diferencial_conservador <- van_conservador - van_sin_proyecto
van_diferencial_base <- van_base - van_sin_proyecto
van_diferencial_ambicioso <- van_ambicioso - van_sin_proyecto

# -------------------------------
# Relación Beneficio/Coste comparada
# -------------------------------
bc_conservador <- sum(beneficios$Beneficio_Conservador_DS) / sum(costes_pgou$Costes_Descontados)
bc_base <- sum(beneficios$Beneficio_Base_DS) / sum(costes_pgou$Costes_Descontados)
bc_ambicioso <- sum(beneficios$Beneficio_Ambicioso_DS) / sum(costes_pgou$Costes_Descontados)

# -------------------------------
# RESULTADO RESUMIDO
# -------------------------------
evaluacion_financiera_comparada <- data.frame(
  Escenario = c("Conservador", "Base", "Ambicioso"),
  VAN_Millones_EUR = c(van_conservador, van_base, van_ambicioso) / 1e6,
  VAN_Diferencial_vs_Sin_Proyecto = c(van_diferencial_conservador, van_diferencial_base, van_diferencial_ambicioso) / 1e6,
  Relacion_Beneficio_Coste = c(bc_conservador, bc_base, bc_ambicioso)
)

print(evaluacion_financiera_comparada)

#############################################
# VISUALIZACIÓN: FLUJO NETO ANUAL COMPARADO
#############################################

# -------------------------------
# Flujo neto anual con proyecto
# -------------------------------
flujo_neto <- data.frame(
  Anio = beneficios$Anio,
  Conservador = beneficios$Beneficio_Conservador_DS - costes_pgou$Costes_Descontados,
  Base = beneficios$Beneficio_Base_DS - costes_pgou$Costes_Descontados,
  Ambicioso = beneficios$Beneficio_Ambicioso_DS - costes_pgou$Costes_Descontados
)

# Flujo neto sin proyecto (negativo, ya que son costes sociales)
flujo_neto_sin_proyecto <- data.frame(
  Anio = sin_proyecto$Anio,
  BAU = -sin_proyecto$Coste_Oportunidad_DS
)

# Combinar ambos para visualización
flujo_neto_comb <- merge(flujo_neto, flujo_neto_sin_proyecto, by = "Anio")

# Convertir a formato largo
flujo_neto_largo <- flujo_neto_comb %>%
  pivot_longer(cols = c("Conservador", "Base", "Ambicioso", "BAU"),
               names_to = "Escenario",
               values_to = "Flujo_Neto")

# -------------------------------
# GRAFICAR COMPARACIÓN
# -------------------------------
ggplot(flujo_neto_largo, aes(x = Anio, y = Flujo_Neto / 1e6, color = Escenario)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  labs(
    x = "Año",
    y = "Millones de €",
    color = "Escenario"
  ) +
  theme_minimal()








#############################################
# BENEFICIOS DE ZONAS VERDES Y HUELLA ECOLÓGICA
#############################################

# Supuestos clave
area_intercambiador <- 10000  # m²
proporcion_intercambiador <- 0.10
area_total_urbanizacion <- area_intercambiador / proporcion_intercambiador  # = 265,000 m²

# Porcentaje de zonas verdes (según PGOU)
porcentaje_verde_actual <- 0.225
porcentaje_verde_futuro <- 0.377

# Cálculo de superficie verde
area_verde_actual <- area_total_urbanizacion * porcentaje_verde_actual
area_verde_futura <- area_total_urbanizacion * porcentaje_verde_futuro
area_verde_incremento <- area_verde_futura - area_verde_actual

# Valor de zonas verdes (€/m²/año)
valor_verde_m2_anual <- 4  # basado en estudios europeos

# Beneficio anual por zonas verdes nuevas
beneficio_verde_anual <- area_verde_incremento * valor_verde_m2_anual

# Horizonte temporal del proyecto
anios <- 2026:2050
tasa_descuento <- 0.03
factor_descuento <- 1 / (1 + tasa_descuento)^(anios - anios[1])

# Flujos de beneficios descontados
beneficio_verde_flujo <- rep(beneficio_verde_anual, length(anios))
beneficio_verde_ds <- beneficio_verde_flujo * factor_descuento

# Total valor actual neto de beneficios por zonas verdes
van_verde <- sum(beneficio_verde_ds)

# ------------------------------
# HUELLA DE CONSTRUCCIÓN DEL INTERCAMBIADOR
# ------------------------------
emision_m2_construccion <- 600  # kg CO₂e / m²
factor_precio_CO2 <- 0.10       # €/kg CO₂e (conservador)

emisiones_construccion_kg <- area_intercambiador * emision_m2_construccion
costo_huella_construccion <- emisiones_construccion_kg * factor_precio_CO2

# ------------------------------
# RESULTADOS
# ------------------------------
cat("Área total del ámbito de intervención:", area_total_urbanizacion, "m²\n")
cat("Área verde actual:", round(area_verde_actual), "m²\n")
cat("Área verde futura:", round(area_verde_futura), "m²\n")
cat("Incremento de zonas verdes:", round(area_verde_incremento), "m²\n")
cat("Beneficio verde anual estimado:", round(beneficio_verde_anual), "€\n")
cat("Valor actual de beneficios verdes (VAN):", round(van_verde), "€\n\n")

cat("Emisiones de construcción estimadas:", round(emisiones_construccion_kg), "kg CO₂e\n")
cat("Costo ambiental estimado de la construcción:", round(costo_huella_construccion), "€\n")






