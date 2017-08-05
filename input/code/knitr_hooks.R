library(knitr)

# pdf.options(useDingbats = TRUE)
# pdf.options(encoding='Cyrillic')

# knit_hooks$set(chunk = function(x, options) x) # do not wrap output in kframe

fra_pdf = function(file, width, height) {
  cairo_pdf(file, width = width, height = height, pointsize = 10, family = "PT Sans")
  # svglite::svglite(file, width = width, height = height, pointsize = 10)
}


opts_chunk$set(list(echo=FALSE,
                    eval=TRUE,
                    cache=FALSE,
                    warning=FALSE,
                    message=FALSE,
                    # cache.path="~/fao_cache/regional15/",
                    dev="fra_pdf",
                    fig.ext='pdf',
                    results='asis')
)

# Why we have to short_text_ChartPage and short_text_ChartPage2 hooks etc......

knit_hooks$set(bar1_smartphone = function(before, options, envir) {
  
  # challenge with hooks was that when eval=FALSE hook was still executed and produced error
  # Added the following line for each hook that stops the function IF there is FALSE in eval either
  # written directly or passed through the spread condition region gadget thing..
  
  # if (options$short_text_ChartPage == FALSE) return()
  # Set fig size
  fig_width  =  8
  fig_height =  8
  if (!before) {
    return(paste0("\\textbf{ ",cntryname," } \n",
                  "\\textit{",cntryname,"} \n"))
  }
})

