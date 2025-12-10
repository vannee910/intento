# -----------------------------------------------------------------------------
# SCRIPT: funciones.R (CORREGIDO FINAL)
# -----------------------------------------------------------------------------

# =============================================================================
# MÓDULO 1: CÁLCULO DE CAPITAL NETO
# =============================================================================
calcular_capital_neto <- function(datos_banco, fecha_analisis) {
  
  datos_filtrados <- datos_banco %>% filter(fecha == fecha_analisis)
  
  buscar_monto <- function(codigo) {
    if (is.na(codigo)) return(0)
    val <- datos_filtrados %>% 
      filter(concepto == codigo) %>% 
      summarise(suma = sum(importe1, na.rm = TRUE)) %>% 
      pull(suma)
    if (length(val) == 0) return(0) else return(val)
  }
  
  reporte <- plantilla_cn_base %>%
    mutate(Monto = sapply(Codigo_Banxico, buscar_monto))
  
  get_val <- function(cod) { sum(reporte$Monto[reporte$Codigo_Banxico %in% cod], na.rm = TRUE) }
  
  # --- Variables de Insumo ---
  asrc_totales  <- get_val(90001)
  asrc_estandar <- get_val(90002)
  asrc_interno  <- get_val(90003)
  
  # --- Cálculo Reservas ---
  res_adm_int <- get_val(95691)
  perd_esp_int <- get_val(95695)
  exceso_int <- max(0, res_adm_int - perd_esp_int)
  limite_int <- 0.006 * asrc_interno 
  reservas_computables_int <- min(exceso_int, limite_int)
  
  res_adm_est <- get_val(95704)
  perd_esp_est <- get_val(95708)
  exceso_est <- max(0, res_adm_est - perd_esp_est)
  limite_est <- 0.0125 * asrc_estandar
  reservas_computables_est <- min(exceso_est, limite_est)
  
  total_reservas_computables <- reservas_computables_int + reservas_computables_est
  
  # --- Capitales ---
  c_contribuido <- get_val(c(95005, 95010, 95035))
  c_ganado      <- get_val(c(95045, 95050, 95055, 95060, 95068))
  deducciones   <- get_val(c(95150, 95315, 95380, 93916))
  
  cap_fundamental <- c_contribuido + c_ganado - deducciones
  cap_basico_no_fund <- get_val(c(95075, 95550))
  cap_basico <- cap_fundamental + cap_basico_no_fund
  
  cap_complementario_instrumentos <- get_val(c(95080, 95555))
  cap_complementario <- cap_complementario_instrumentos + total_reservas_computables
  cap_neto <- cap_basico + cap_complementario
  
  # --- Índices ---
  coef_fundamental <- if(asrc_totales > 0) cap_fundamental / asrc_totales else 0
  coef_basico      <- if(asrc_totales > 0) cap_basico / asrc_totales else 0
  icap             <- if(asrc_totales > 0) cap_neto / asrc_totales else 0
  
  # --- Inyección Final ---
  reporte_final <- reporte %>%
    mutate(Monto = case_when(
      Concepto == "I. CAPITAL CONTRIBUIDO" ~ c_contribuido,
      Concepto == "II. CAPITAL GANADO" ~ c_ganado,
      Concepto == "III. INVERSIONES Y DEDUCCIONES" ~ deducciones,
      Concepto == "CAPITAL FUNDAMENTAL" ~ cap_fundamental,
      Concepto == "PARTE BÁSICA NO FUNDAMENTAL" ~ cap_basico_no_fund,
      Concepto == "CAPITAL BÁSICO" ~ cap_basico,
      Concepto == "PARTE COMPLEMENTARIA" ~ cap_complementario,
      Concepto == "CAPITAL COMPLEMENTARIO" ~ cap_complementario,
      Concepto == "CAPITAL NETO" ~ cap_neto,
      grepl("Reservas Computables.*Interno", Concepto) ~ reservas_computables_int,
      grepl("Reservas Computables.*Estándar", Concepto) ~ reservas_computables_est,
      Concepto == "COEFICIENTE DE CAPITAL FUNDAMENTAL" ~ coef_fundamental,
      Concepto == "COEFICIENTE DE CAPITAL BÁSICO" ~ coef_basico,
      Concepto == "ÍNDICE DE CAPITALIZACIÓN (ICAP)" ~ icap,
      TRUE ~ Monto 
    )) %>%
    select(Concepto, Monto)
  
  return(reporte_final)
}

# =============================================================================
# MÓDULO 2: GRÁFICAS
# =============================================================================
graficas_CN <- function(datos_banco) {
  datos_plot <- datos_banco %>% 
    mutate(fecha = as.Date(fecha)) %>% 
    arrange(fecha)
  
  g1 <- ggplot(filter(datos_plot, concepto == 90001), aes(x = fecha, y = importe1)) +
    geom_line(color = "#2874A6", linewidth = 1) + 
    geom_point(color = "#1B4F72") +
    labs(title = "Evolución Activos Sujetos a Riesgo", 
         subtitle = "Histórico", y = "MXN", x = "Fecha") + 
    theme_minimal()
  
  datos_comp <- filter(datos_plot, concepto %in% c(95005, 95050)) %>% 
    mutate(Tipo = ifelse(concepto == 95005, "Capital Social", "Rdos. Acumulados"))
  
  g2 <- ggplot(datos_comp, aes(x = as.factor(fecha), y = importe1, fill = Tipo)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c("#AED6F1", "#2E86C1")) +
    labs(title = "Estructura de Capital", 
         subtitle = "Comparativa: Social vs Resultados", x = "Fecha", y = "Monto") + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(g1)
  print(g2)
  
  return(list(riesgo = g1, estructura = g2))
}

# =============================================================================
# MÓDULO 2: CÁLCULO ACT. PONDERADOS (AQUÍ ESTABA EL ERROR, YA ELIMINADO)
# =============================================================================
act_pond <- function(datos_credito_base) {
  
  names(datos_credito_base) <- tolower(names(datos_credito_base))
  
  buscar_col <- function(patron) {
    grep(patron, names(datos_credito_base), value = TRUE)[1]
  }
  
  col_monto <- buscar_col("monto.*dispuesto|importe|saldo")
  col_pond  <- buscar_col("ponderador")
  col_int   <- buscar_col("interes.*devengado")
  
  if(is.na(col_monto) | is.na(col_pond)) stop("ERROR: No encuentro columnas de Monto o Ponderador.")
  
  df <- datos_credito_base %>%
    rename(monto = all_of(col_monto), pond_raw = all_of(col_pond)) %>%
    mutate(interes = if(!is.na(col_int)) get(col_int) else 0)
  
  limpiar_pct <- function(x) {
    if(is.numeric(x)) return(x)
    val <- as.numeric(gsub("%", "", x))
    if(!is.na(val) && val > 2) return(val/100) else return(val)
  }
  
  df_calc <- df %>%
    mutate(
      exposicion = replace_na(monto, 0) + replace_na(interes, 0),
      Ponderador_Num = sapply(pond_raw, limpiar_pct),
      activos_pond = exposicion * Ponderador_Num,
      req_capital = activos_pond * 0.08
    )
  
  # 1. Tabla Desglose (Eliminamos la creación de texto duplicada)
  tabla_desglose <- df_calc %>%
    group_by(Ponderador_Num) %>%
    summarise(
      Monto_Total = sum(exposicion, na.rm = TRUE),
      Capital_Req_Total = sum(req_capital, na.rm = TRUE)
    ) %>%
    ungroup() # <--- ¡CORREGIDO! Ya no creamos Ponderador_Texto aquí
  
  # 2. Tabla Totales
  tabla_totales <- tibble(
    Concepto = c("Total Activos Ponderados", "Total Requerimiento Capital"),
    Monto = c(sum(df_calc$activos_pond, na.rm = TRUE),
              sum(df_calc$req_capital, na.rm = TRUE))
  )
  
  return(list(desglose = tabla_desglose, totales = tabla_totales))
}

# =============================================================================
# MÓDULO 2: VOLCADO DE DATOS A EXCEL
# =============================================================================

volcar_cn_plantilla <- function(wb, datos_cn, nombre_hoja = "Capital_Neto") {
  if(!nombre_hoja %in% names(wb)) addWorksheet(wb, nombre_hoja)
  writeData(wb, sheet = nombre_hoja, x = datos_cn)
  addStyle(wb, sheet = nombre_hoja, style = createStyle(numFmt = "#,##0.00"), 
           rows = 2:(nrow(datos_cn)+1), cols = 2)
  return(wb)
}

reporte_pond <- function(wb, resultados_pond, nombre_hoja = "REPORTE") {
  
  if(!nombre_hoja %in% names(wb)) {
    addWorksheet(wb, nombre_hoja)
  }
  
  # Usamos mapa_ponderadores (de objetos.R) para poner el texto bonito
  tabla_visual <- mapa_ponderadores %>%
    left_join(resultados_pond$desglose, by = "Ponderador_Num") %>%
    mutate(
      Importe = replace_na(Monto_Total, 0),
      Capital_Neto_Requerido = replace_na(Capital_Req_Total, 0)
    ) %>%
    select(Ponderador_Texto, Importe, Capital_Neto_Requerido)
  
  # Volcamos desglose
  writeData(wb, sheet = nombre_hoja, x = tabla_visual, 
            startRow = 7, startCol = 2, colNames = FALSE)
  
  # Volcamos totales debajo
  fila_totales <- 7 + nrow(tabla_visual) + 1
  writeData(wb, sheet = nombre_hoja, x = resultados_pond$totales, 
            startRow = fila_totales, startCol = 2, colNames = FALSE)
  
  return(wb)
}

