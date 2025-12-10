# -----------------------------------------------------------------------------
# SCRIPT: main.R
# DESCRIPCIÓN: Ejecución del proyecto final
# -----------------------------------------------------------------------------

rm(list = ls()) # Limpiar entorno
source("scripts/objetos.R")
source("scripts/funciones.R")

# --- 1. CONFIGURACIÓN ---
fecha_objetivo <- as.Date("2012-12-31")
path_datos_cn  <- "data/capitalneto_proyecto.xlsx"
path_datos_rc  <- "data/riesgo_credito.xlsx"
path_plantilla <- "docs/reporte.xlsx" # Tu archivo plantilla limpio

# Verificar existencia de archivos clave
if(!file.exists(path_datos_cn)) stop("Falta archivo Capital Neto")
if(!file.exists(path_datos_rc)) stop("Falta archivo Riesgo Crédito")
if(!file.exists(path_plantilla)) {
  # Si no existe la plantilla, creamos un wb nuevo temporalmente
  warning("No encontré 'reporte.xlsx' en docs. Creando uno nuevo...")
  wb <- createWorkbook()
  addWorksheet(wb, "REPORTE") # Necesaria para riesgo
} else {
  wb <- loadWorkbook(path_plantilla)
}

# --- 2. MÓDULO 1: CAPITAL NETO ---
cat("Calculando Capital Neto...\n")
datos_cn_raw <- read_xlsx(path_datos_cn, sheet = 2) %>% mutate(fecha = as.Date(fecha))

# a) Cálculo (Punto 1)
df_capital_neto <- calcular_capital_neto(datos_cn_raw, fecha_objetivo)

# b) Gráficas (Punto 2)
mis_graficas <- graficas_CN(datos_cn_raw) 
# Nota: Las gráficas se muestran en la ventana de plots de RStudio.

# c) Volcado a Excel (Punto 4)
wb <- volcar_cn_plantilla(wb, df_capital_neto, "Capital_Neto")


# --- 3. MÓDULO 2: RIESGO DE CRÉDITO ---
cat("Calculando Riesgo de Crédito...\n")
datos_credito <- read_xlsx(path_datos_rc, sheet = "BASE")

# a) Cálculo de Activos Ponderados (Punto 5)
# Genera lista con: $desglose y $totales
resultados_riesgo <- act_pond(datos_credito)

# b) Volcado a Excel (Punto 7)
# Usa la función reporte_pond definida en funciones.R
wb <- reporte_pond(wb, resultados_riesgo, "REPORTE")


# --- 4. GUARDAR RESULTADO FINAL ---
saveWorkbook(wb, path_plantilla, overwrite = TRUE)

cat("------------------------------------------------------\n")
cat("¡PROCESO FINALIZADO CON ÉXITO!\n")
cat("El archivo actualizado se encuentra en:", path_plantilla, "\n")