message(paste(rep("-", 100), collapse = ""))
message("\tLeyendo mortalidad de pensionistas")

# Carga de beneficiarios de RTR----------------------------------------------------------------------
load(paste0(parametros$RData_seg, "IESS_RTR_rentas.RData"))
# Transición de activos a jubilados por incapacidad permanente adsoluta y total----------------------
fallecidos_abso_total <- base %>%
  filter(
    tipo_seguro == "RT", tipo_prestacion %in% c("PA", "PT", "PP"),
    !is.na(fecha_defuncion)
  ) %>%
  filter(
    fecha_defuncion > as.Date("2011-12-31", "%Y-%m-%d"),
    fecha_defuncion < as.Date("2019-01-01", "%Y-%m-%d")
  ) %>%
  distinct(cedula, tipo_prestacion, .keep_all = TRUE) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
    enddate = fecha_defuncion,
    units = "years",
    precise = TRUE
  ))) %>%
  group_by(edad) %>%
  mutate(fallecidos = n()) %>%
  ungroup() %>%
  distinct(edad, .keep_all = TRUE) %>%
  select(edad, fallecidos) %>%
  arrange(edad)

# Jubilados de incap permanente total y absoluta, entre 2012 y 2018----------------------------------
load(paste0(parametros$RData_seg, "IESS_RTR_rentas.RData"))

jubilados_abso_total <- base %>%
  distinct(cedula, anio, mes, tipo_seguro, tipo_prestacion, .keep_all = TRUE) %>%
  filter(tipo_prestacion %in% c("PA", "PT", "PP")) %>%
  filter(
    fecha_siniestro > as.Date("31/12/2011", "%d/%m/%Y"),
    fecha_siniestro < as.Date("01/01/2019", "%d/%m/%Y")
  ) %>%
  mutate(edad = round(age_calc(fecha_nacimiento,
    enddate = as.Date(paste0("30/06/", anio), "%d/%m/%Y"),
    units = "years",
    precise = TRUE
  ))) %>%
  distinct(cedula, anio, .keep_all = TRUE) %>%
  group_by(edad) %>%
  mutate(pensionistas = n()) %>%
  ungroup() %>%
  distinct(edad, .keep_all = TRUE) %>%
  select(edad, pensionistas) %>%
  arrange(edad)

# Probailidad de jubilarse por incapacidad permanente adsoluta y total, dado que se está activo------
message("\tCalculando transición entre jubilado y muerto")
mortalidad_jubilados_edad_rtr <- left_join(jubilados_abso_total, fallecidos_abso_total,
  by = "edad"
) %>%
  mutate(tasa_mortalidad = fallecidos / pensionistas) %>%
  filter(edad >= 18)


# Guardando mortalidad -----------------------------------------------------------------------------
message("\tGuardando tablas")
save(mortalidad_jubilados_edad_rtr,
  file = paste0(parametros$RData_seg, "IESS_RTR_mortalidad_pensionistas.RData")
)

# Borrando Dataframes--------------------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()
