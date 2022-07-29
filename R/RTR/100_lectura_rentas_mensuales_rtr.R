message(paste(rep("-", 100), collapse = ""))

message("\tLectura de pagos de pensiones de permanetes anbsolutas y total, y montepío de RTR")

# Carga información registro civil-------------------------------------------------------------------
load("Z:/IESS/Data/rtr/RC_fecha_defuncion.Rdata")

# Carga de archivos de rentas de RTR-----------------------------------------------------------------
base <- 1
for (anio in 2012:2018) {
  for (mes in 1:12) {
    archivo <- paste0("nomina_", anio, "_", mes, ".txt")
    file <- paste0(parametros$Data_seg, "IESS_RTR_rentas/", archivo)
    # Carga del pago de pensiones mensuales de las rentas de RTR----------------------------------
    aux <- read.table(file,
      sep = "\t",
      dec = ".",
      na = "",
      header = TRUE,
      skip = 0
    ) %>%
      clean_names() %>%
      select(
        "anio",
        "mes",
        "cedula",
        "tipo_seguro",
        "tipo_prestacion",
        "sexo",
        "fecha_nacimiento",
        "beneficiario",
        "renta_mensual",
        "liq_pagar",
        "tot_ingr",
        "tot_desc",
        "tot_pension"
      )
    if (dim(aux) == "NULL") {
      base <- aux
    } else if (dim(aux)[1] > 0) {
      base <- rbind(base, aux)
    }
  }
}

# Depuración de base bruta---------------------------------------------------------------------------
base$mes <- gsub("Enero", "1", base$mes)
base$mes <- gsub("Febrero", "2", base$mes)
base$mes <- gsub("Marzo", "3", base$mes)
base$mes <- gsub("Abril", "4", base$mes)
base$mes <- gsub("Mayo", "5", base$mes)
base$mes <- gsub("Junio", "6", base$mes)
base$mes <- gsub("Julio", "7", base$mes)
base$mes <- gsub("Agosto", "8", base$mes)
base$mes <- gsub("Septiembre", "9", base$mes)
base$mes <- gsub("Octubre", "10", base$mes)
base$mes <- gsub("Noviembre", "11", base$mes)
base$mes <- gsub("Diciembre", "12", base$mes)
base$mes <- as.integer(base$mes)


base <- (base[-1, ]) %>%
  left_join(., fallecidos, by = "cedula") %>%
  mutate(
    sexo = if_else(sexo == "Femenino", "F", "M"),
    fecha_nacimiento = as.Date(fecha_nacimiento, "%d/%m/%Y"),
    fecha_nacimiento = if_else(is.na(fecha_nacimiento),
      as.Date("1967/07/07", "%Y/%m/%d"),
      fecha_nacimiento
    ),
    fecha_siniestro = as.Date(paste0(anio, "/", mes, "/", "01"), "%Y/%m/%d")
  ) %>%
  mutate(edad_siniestro = round(age_calc(fecha_nacimiento,
    enddate = fecha_siniestro,
    units = "years",
    precise = TRUE
  )))

# Filtrar prestación permanete total-----------------------------------------------------------------
prestaciones_pt <- base %>%
  filter(tipo_prestacion == "PT")
# left_join(.,calculo_rentas_rtr,by=c('cedula'='asegurado'))
# group_by(anio) %>%
# mutate(suma=sum(tot_ingr,na.rm = TRUE)) %>%
# distinct(anio, .keep_all = TRUE) %>%
# select(anio,suma)

# Filtrar prestaciones permanente absoluta-----------------------------------------------------------
prestaciones_pa <- base %>%
  filter(tipo_prestacion == "PA")
# left_join(.,calculo_rentas_rtr,by=c('cedula'='asegurado'))

# Filtar prestaciones de montepío viudez-------------------------------------------------------------
prestaciones_viudez <- base %>%
  filter(tipo_prestacion == "VO", beneficiario == "VIUDEDAD")

# Filtar prestaciones de montepío viudez-------------------------------------------------------------
prestaciones_orfandad <- base %>%
  filter(tipo_prestacion == "VO", beneficiario == "ORFANDAD")

# Guardando en un Rdata------------------------------------------------------------------------------
message("\tGuardando Rdatas")

save(base,
  prestaciones_pt,
  prestaciones_pa,
  prestaciones_viudez,
  prestaciones_orfandad,
  file = paste0(parametros$RData_seg, "IESS_RTR_rentas.RData")
)

# Borrando data.frames-------------------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% "parametros")])
gc()
