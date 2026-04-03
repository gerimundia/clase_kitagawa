
# Configuracion -----


# install.packages("pacman")
pacman::p_load(dplyr, tidyr)


# Datos -----
load("datos.RData")


df_wide <- datos %>%
  pivot_wider(
    names_from=poblacion,
    values_from=c(w, r)) %>% 
  filter(estatus_migratorio!="Todos")


# Calculos intermedios -----

df_calc <- df_wide %>%
  mutate(
    w_bar=(w_A + w_B)/2,
    r_bar=(r_A + r_B)/2,
    diff_w=w_A - w_B,
    diff_r=r_A - r_B,
    efecto_tasas_i=diff_w * r_bar,
    efecto_composicion_i=diff_r * w_bar,
    contribucion_total=efecto_tasas_i + efecto_composicion_i)

tasa_A <- sum(df_calc$w_A * df_calc$r_A)
tasa_B <- sum(df_calc$w_B * df_calc$r_B)

diferencia <- tasa_A - tasa_B

composicion <- sum(df_calc$efecto_composicion_i)
tasas <- sum(df_calc$efecto_tasas_i)

check <- composicion + tasas

print(c(check, diferencia))


# Resultados -----

resultados <- data.frame(
  Concepto = c(
    "Tasa A",
    "Tasa B",
    "Diferencia",
    "Efecto composicion",
    "Efecto tasas",
    "Contribucion composición (%)",
    "Contribución tasas (%)"
  ),
  Valor = c(
    tasa_A,
    tasa_B,
    diferencia,
    composicion,
    tasas,
    (composicion/diferencia * 100),
    (tasas/diferencia * 100)
  )
)

print(resultados)

