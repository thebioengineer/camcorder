# Save PDFs with custom fonts

[camcorder](https://thebioengineer.github.io/camcorder/) doesn’t only
work with raster images such as PNG and JPEG but also PDF files. There
are some benefits of using the PDF format when saving ggplot output:
vector graphics are *lossless*, can be converted easily in raster
formats of any resolution and also be manipulated afterwards in a vector
design tool. Furthermore, the PDF format often supports the use of
custom fonts (when using the Cairo device)[^1].

To automatically save your graphics in PDF format, just change the
device setting to `cairo_pdf`[^2] when recording your ggplot code:

``` r

library(ggplot2)
library(camcorder)

gg_record(
  dir = file.path(tempdir(), "recording"), 
  device = cairo_pdf, # we need to set the Cairo device
  width = 8,
  height = 5
)
```

*Mac users should ensure that [XQuartz is
installed](https://www.xquartz.org/) which is needed to use the cairo
pdf device.*

To supply custom fonts in R, the respective font needs to be installed
locally on the system.

You can make sure the font file is installed by using the
`system_fonts()` from the latest standard, the `{systemfont}` package:

``` r

systemfonts::system_fonts()
#> # A tibble: 162 × 10
#>    path          index name  family style weight width italic monospace variable
#>    <chr>         <int> <chr> <chr>  <chr> <ord>  <ord> <lgl>  <lgl>     <lgl>   
#>  1 /usr/share/f…     0 URWG… URW G… Book… normal norm… FALSE  FALSE     FALSE   
#>  2 /usr/share/f…     0 Lato… Lato   Thin… normal norm… TRUE   FALSE     FALSE   
#>  3 /usr/share/f…     0 Libe… Liber… Bold  bold   norm… FALSE  FALSE     FALSE   
#>  4 /usr/share/f…     0 Lato… Lato   Semi… semib… norm… TRUE   FALSE     FALSE   
#>  5 /usr/share/f…     0 Nimb… Nimbu… Bold… bold   norm… TRUE   TRUE      FALSE   
#>  6 /usr/share/f…     0 Libe… Liber… Bold  bold   norm… FALSE  TRUE      FALSE   
#>  7 /usr/share/f…     0 Nimb… Nimbu… Bold… bold   norm… TRUE   FALSE     FALSE   
#>  8 /usr/share/f…     0 Nimb… Nimbu… Regu… normal norm… FALSE  FALSE     FALSE   
#>  9 /usr/share/f…     0 Lato… Lato   Medi… medium norm… TRUE   FALSE     FALSE   
#> 10 /usr/share/f…     0 Nimb… Nimbu… Ital… normal norm… TRUE   FALSE     FALSE   
#> # ℹ 152 more rows
```

You can simply filter this tibble for any font:[^3]

``` r

systemfonts::system_fonts() |> 
  dplyr::filter(grepl("Dyna", family)) |>
  dplyr::pull(name) |> 
  sort()
#> character(0)
```

Now let’s create a graphic with the DynaPuff Condensed typeface as the
`base_family` of our theme:

``` r

g <- 
  ggplot(diamonds, aes(x = cut)) + 
  geom_bar(fill = "grey65") +
  theme_minimal(
    base_family = "DynaPuff Condensed",
    base_size = 24
  )

g
```

![](custom_font.png)

And now let’s add a non-condensed, bold title:

``` r

g +
  ggtitle("PDFs are a font lovers best friend") +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(family = "DynaPuff", face = "bold")
  )
```

![](custom_font_title.png)

That’s it. If you want to know more about good practices how to handle
and customize fonts in [ggplot2](https://ggplot2.tidyverse.org) check
this [blog post by June
Choe](https://yjunechoe.github.io/posts/2021-06-24-setting-up-and-debugging-custom-fonts/).

[^1]: Thanks to the [ragg](https://ragg.r-lib.org) package custom font
    support is working quite well for raster images now—but it’s not a
    vector graphic 🙃

[^2]: [Cairo](https://en.wikipedia.org/wiki/Cairo_(graphics)) is an
    open-source graphics library that is [known to work very well with
    custom
    fonts](https://www.andrewheiss.com/blog/2017/09/27/working-with-r-cairo-graphics-custom-fonts-and-ggplot/).

[^3]: To make the difference obvious we use the fun, quirky font
    [DynaPuff](https://github.com/googlefonts/dynapuff) which is freely
    available.
