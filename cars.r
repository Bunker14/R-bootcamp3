# Paso 1: Cargar las librerías necesarias
library(dplyr)
library(tidyr)

# Paso 2: Cargar el dataset mtcars y convertirlo en un dataframe
data(mtcars)
df <- as.data.frame(mtcars)

# Paso 3: Selección de columnas y filtrado de filas
df_filtrado <- df %>%
  select(mpg, cyl, hp, gear) %>%
  filter(cyl > 4)

# Verificar resultado de la selección y filtrado
print("Datos seleccionados y filtrados:")
print(df_filtrado)

# Paso 4: Ordenación y renombrado de columnas
df_ordenado <- df_filtrado %>%
  arrange(desc(hp)) %>%
  rename(consumo = mpg, potencia = hp)

# Verificar resultado de ordenación y renombrado
print("Datos ordenados y columnas renombradas:")
print(df_ordenado)

# Paso 5: Creación de nueva columna 'eficiencia' y agregación
df_eficiencia <- df_ordenado %>%
  mutate(eficiencia = consumo / potencia) %>%
  group_by(cyl, gear) %>%  # Aseguramos que gear esté incluido en el group_by
  summarise(consumo_medio = mean(consumo), potencia_maxima = max(potencia), .groups = 'drop')

# Verificar que la columna 'eficiencia' y 'gear' estén presentes
print("Datos con eficiencia calculada y gear incluido:")
print(df_eficiencia)

# Paso 6: Creación del segundo dataframe con gear y tipo_transmision
df_transmision <- data.frame(
  gear = c(3, 4, 5),
  tipo_transmision = c("Manual", "Automática", "Semiautomática")
)

# Paso 7: Realizar un left_join con el dataframe principal
df_completo <- left_join(df_eficiencia, df_transmision, by = "gear")

# Verificar el resultado del left_join
print("Datos después del left_join con tipo_transmision:")
print(df_completo)

# Paso 8: Transformación de formato largo con pivot_longer
df_largo <- df_completo %>%
  pivot_longer(cols = c(consumo_medio, potencia_maxima), names_to = "medida", values_to = "valor")

# Verificar resultado de pivot_longer
print("Datos en formato largo:")
print(df_largo)

# Identificar posibles duplicados: Agrupar por claves
df_largo_agrupado <- df_largo %>%
  group_by(cyl, gear, tipo_transmision, medida) %>%
  summarise(valor_promedio = mean(valor, na.rm = TRUE))

# Verificar resultado de la agrupación y eliminación de duplicados
print("Datos agrupados y sin duplicados:")
print(df_largo_agrupado)

# Paso 9: Transformación de formato ancho con pivot_wider
df_ancho <- df_largo_agrupado %>%
  pivot_wider(names_from = medida, values_from = valor_promedio)

# Verificar resultado de pivot_wider
print("Datos en formato ancho:")
print(df_ancho)
