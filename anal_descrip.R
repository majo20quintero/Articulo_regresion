library(readxl)
library(ggplot2)
library(corrplot)

url <- "https://raw.githubusercontent.com/majo20quintero/Articulo_regresion/main/datos.xlsx"

archivo <- tempfile(fileext = ".xlsx")

download.file(
  url = url,
  destfile = archivo,
  mode = "wb",
  quiet = TRUE
)

datos <- read_excel(archivo)


unlink(archivo)


cat("\n========== DIMENSIONES DE LOS DATOS ==========\n")
cat("Numero de filas:", nrow(datos), "\n")
cat("Numero de columnas:", ncol(datos), "\n")

cat("\n========== NOMBRES DE LAS VARIABLES ==========\n")
print(names(datos))

cat("\n========== ESTRUCTURA DE LOS DATOS ==========\n")
str(datos)

cat("\n========== RESUMEN GENERAL ==========\n")
print(summary(datos))


variables_numericas <- names(datos)[
  sapply(datos, is.numeric)
]

variables_numericas <- setdiff(
  variables_numericas,
  "ID_participante"
)

variables_categoricas <- names(datos)[
  sapply(datos, function(x) {
    is.character(x) || is.factor(x) || is.logical(x)
  })
]


variables_numericas <- variables_numericas[
  !grepl("^ID($|_)", variables_numericas, ignore.case = TRUE)
]


if (length(variables_numericas) > 0) {
  for (v in variables_numericas) {
    grafico <- ggplot(datos, aes(x = .data[[v]])) +
      geom_histogram(
        bins = 20,
        fill = "steelblue",
        color = "white",
        na.rm = TRUE
      ) +
      labs(
        title = paste("Distribucion de", v),
        x = v,
        y = "Frecuencia"
      ) +
      theme_minimal()
    
    print(grafico)
    readline("Presiona ENTER para ver el siguiente grafico...")
  }
}


if (length(variables_numericas) > 0) {
  for (v in variables_numericas) {
    grafico <- ggplot(datos, aes(y = .data[[v]])) +
      geom_boxplot(
        fill = "darkseagreen3",
        na.rm = TRUE
      ) +
      labs(
        title = paste("Diagrama de caja de", v),
        x = "",
        y = v
      ) +
      theme_minimal()
    
    print(grafico)
    readline("Presiona ENTER para ver el siguiente grafico...")
  }
}


variables_barras <- names(datos)[
  sapply(datos, function(x) {
    n_distinct <- length(unique(stats::na.omit(x)))
    (is.character(x) || is.factor(x) || is.logical(x) ||
        (is.numeric(x) && n_distinct > 1 && n_distinct <= 10))
  })
]

variables_barras <- setdiff(
  variables_barras,
  "ID_participante"
)

for (v in variables_barras) {
  grafico <- ggplot(
    datos,
    aes(x = as.factor(.data[[v]]))
  ) +
    geom_bar(fill = "coral", na.rm = TRUE) +
    labs(
      title = paste("Frecuencia de", v),
      x = v,
      y = "Frecuencia"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      )
    )
  
  print(grafico)
  readline("Presiona ENTER para ver el siguiente grafico...")
}

if (length(variables_numericas) >= 2) {
  pares <- utils::combn(
    variables_numericas,
    2,
    simplify = FALSE
  )
  
  
  pares <- head(pares, 15)
  
  for (par in pares) {
    grafico <- ggplot(
      datos,
      aes(
        x = .data[[par[1]]],
        y = .data[[par[2]]]
      )
    ) +
      geom_point(
        color = "steelblue",
        alpha = 0.6,
        na.rm = TRUE
      ) +
      geom_smooth(
        method = "lm",
        se = FALSE,
        color = "red",
        na.rm = TRUE
      ) +
      labs(
        title = paste(par[1], "vs.", par[2]),
        x = par[1],
        y = par[2]
      ) +
      theme_minimal()
    
    print(grafico)
    readline("Presiona ENTER para ver el siguiente grafico...")
  }
}



if (length(variables_numericas) >= 2) {
  
  datos_num <- datos[, variables_numericas, drop = FALSE]
  
  datos_num <- datos_num[
    ,
    sapply(datos_num, function(x) {
      sum(!is.na(x)) > 1 &&
        length(unique(na.omit(x))) > 1
    }),
    drop = FALSE
  ]
  
  if (ncol(datos_num) >= 2) {
    
    matriz_cor <- cor(
      datos_num,
      use = "pairwise.complete.obs"
    )
    
    corrplot::corrplot(
      matriz_cor,
      method = "color",
      type = "upper",
      tl.col = "black",
      tl.srt = 45,
      addCoef.col = "black",
      number.cex = 0.5
    )
    
    readline("Presiona ENTER para terminar...")
  }
}

cat("\n========== ANALISIS FINALIZADO ==========\n")
cat("No se guardaron graficos ni tablas en archivos.\n")

