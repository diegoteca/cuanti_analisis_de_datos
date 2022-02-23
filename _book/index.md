--- 
title: "Análisis de datos"
author: "Metodistas del Sur"
date: "2022-02-23"
site: bookdown::bookdown_site
documentclass: book
bibliography: [book.bib, packages.bib]
# url: your book url like https://bookdown.org/yihui/bookdown
# cover-image: path to the social sharing image like images/cover.jpg
description: |
  This is a minimal example of using the bookdown package to write a book.
  The HTML output format for this example is bookdown::bs4_book,
  set in the _output.yml file.
biblio-style: apalike
csl: chicago-fullnote-bibliography.csl
---

# Análisis de datos {.unnumbered}

El siguiente informe tiene una finalidad principalmente pedagógica. Específicamente está pensado como un mecanismo para ayudar a alcanzar los siguientes objetivos que forman parte del [programa de la materia](https://docs.google.com/document/d/15ZuHJ1ZM7Z0g0Edt-mv1PCB697-x6-rZfcWdAtd85yM/edit#heading=h.s43n504lcmmx "programa de la materia"):

OE4 - *Conocimiento* mínimo de la existencia y pertinencia de las técnicas de análisis de datos *básicas* y una *habilidad* mínima en la ejecución de las mismas.

OE5 - *Conocimiento* mínimo de la existencia y pertinencia de (otras) técnicas de análisis de datos más específicas y (usualmente) más *complejas*.

Aparte de la distinción entre técnicas básicas y complejas la diferencia pedagógica entre ambos objetivos es que las técnicas complejas se aspira sólo a conocerlas mientras que en las básicas se espera que se adquiera cierta habilidad en su ejecución. Expresado de otro modo, de las técnicas consideradas complejas se espera que se sepa de su existencia y qué tipos de problemas ayuda a solucionar. En cambio, en las técnicas básicas se espera lo anterior pero que además se adquiera la habilidad de poder ejecutar la misma.

En cuanto al estilo del texto, si bien van a encontrar citas y referencias, es deliberadamente informal.

To render this example to PDF as a `bookdown::pdf_book`, you'll need to install XeLaTeX. You are recommended to install TinyTeX (which includes XeLaTeX): <https://yihui.org/tinytex/>.

```r
bookdown::render_book()
```



```r
bookdown::serve_book()
```

