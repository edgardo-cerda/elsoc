library(tidyverse)

# Función para limpiar datos string de forma resumida:
limpiar_str <- function(x) {
    y <- x %>%
        stringr::str_squish() %>%
        stringr::str_replace_all(c("á" = "a", "é" = "e", "í" = "i", "ó" = "o", "ú" = "u", "ñ" = "nn", "Ñ" = "NN","ü" = "u", "'" = "","Á" = "A","É" = "E","Í" = "I","Ó" = "O","Ú" = "U"))
    return(ifelse(y == 'NA', '-', y))
}

variable_list <- readxl::read_excel(path = file.path('..', '..', '..',
                                                     '1_Cuestionario_y_Listado_Variables',
                                                     '2_Listado_de_variables_definitivo',
                                                     '0A_Listado_Variables_Global_ELSOC_v2021jul.xlsx'),
                                        sheet = '2_Items_Total') %>%
    janitor::clean_names() %>%
    mutate(variable = codigo_longitudinal,
           label = limpiar_str(etiqueta),
           module = limpiar_str(modulo),
           general_concept = limpiar_str(concepto_general),
           question = paste(limpiar_str(fraseo_pregunta), limpiar_str(fraseo_item)),
           question = ifelse(question == '- -', '-', question)) %>%
    select(variable, label, module, general_concept, question, opciones_respuesta, codigos_respuesta,
           tipo_variable, niveles, starts_with('ola_')) %>%
    mutate(across(starts_with('ola_'), ~{ifelse( . == 'Si', 'Yes', .)}))

usethis::use_data(variable_list, variable_list, overwrite = TRUE)


