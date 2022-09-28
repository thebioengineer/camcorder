#' @description preview generated images in a an html page and open viewer
#' @noRd
preview_film <- function(){

  records <- get_file_records()

  if (tolower(GG_RECORDING_ENV$device_ext) == "pdf") {

    system2('open',
            args = c(
              shQuote(file.path(GG_RECORDING_ENV$recording_dir, records[length(records)])
            )),
            wait = FALSE)

  } else{
    image_viewer_html <-
      file.path(tempdir(), "preview.html")

    film_cyclotron(IMAGE = records[length(records)],
                   filename = image_viewer_html)

    viewer <- getOption("viewer", utils::browseURL)

    if (is.function(viewer) &&
        length(image_viewer_html) > 0 && interactive()) {
      viewer(image_viewer_html)
    }
  }

  invisible()

}

#' get list of recorded files in recording dir
#' @noRd
get_file_records <- function(full_path = FALSE){

  file_preview_ext <- paste0("[.]", GG_RECORDING_ENV$device_ext, "$")

  file_preview_format <- "\\d{4}_\\d{2}_\\d{2}_\\d{2}_\\d{2}_\\d{2}[.]\\d+"

  list.files(
    path    = GG_RECORDING_ENV$recording_dir,
    pattern = paste0("^",file_preview_format,file_preview_ext),
    full.names = full_path
  )

}


#' @title Generate HTML
#' @importFrom tools file_ext
#' @importFrom jsonlite base64_enc
#' @noRd
#'
film_cyclotron <- function(IMAGE,filename = tempfile(fileext = ".html")){

  image_path <- file.path(GG_RECORDING_ENV$recording_dir, IMAGE)
  image_ext <- file_ext(IMAGE)

  if(tolower(image_ext) %in% c("tiff","emf","eps","ps")){
    temp_image <- tempfile(fileext = ".png")
    image <- magick::image_read(image_path)
    converted_image <- magick::image_convert(image,format = "png")
    magick::image_write(converted_image, path = temp_image)
    image_path <- temp_image
    image_ext <- "png"
  }

  b64 <-  paste0("data:image/",image_ext,";base64," , base64_enc(readBin(image_path, "raw", file.info(image_path)[1, "size"])))

  viewer_html <- c(
    "<div style='height:100%;width:100%;'>",
    paste0("<img class='zoom' src=\"",b64,"\" style = \"max-width:100%;max-height:100%;width:auto;height;auto;display: block;margin-left: auto;margin-right: auto; vertical-align:middle;\"/>"),
    "</div>",
    "<script>",
    "
    /*!
      Wheelzoom 4.0.1
    license: MIT
    http://www.jacklmoore.com/wheelzoom
    */
      window.wheelzoom = (function(){
        var defaults = {
          zoom: 0.10,
          maxZoom: false,
          initialZoom: 1,
          initialX: 0.5,
          initialY: 0.5,
        };

        var main = function(img, options){
          if (!img || !img.nodeName || img.nodeName !== 'IMG') { return; }

          var settings = {};
          var width;
          var height;
          var bgWidth;
          var bgHeight;
          var bgPosX;
          var bgPosY;
          var previousEvent;
          var transparentSpaceFiller;

          function setSrcToBackground(img) {
            img.style.backgroundRepeat = 'no-repeat';
            img.style.backgroundImage = 'url(\"'+img.src+'\")';
            transparentSpaceFiller = 'data:image/svg+xml;base64,'+window.btoa('<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"'+img.naturalWidth+'\" height=\"'+img.naturalHeight+'\"></svg>');
            img.src = transparentSpaceFiller;
          }

          function updateBgStyle() {
            if (bgPosX > 0) {
              bgPosX = 0;
            } else if (bgPosX < width - bgWidth) {
              bgPosX = width - bgWidth;
            }

            if (bgPosY > 0) {
              bgPosY = 0;
            } else if (bgPosY < height - bgHeight) {
              bgPosY = height - bgHeight;
            }

            img.style.backgroundSize = bgWidth+'px '+bgHeight+'px';
            img.style.backgroundPosition = bgPosX+'px '+bgPosY+'px';
          }

          function reset() {
            bgWidth = width;
            bgHeight = height;
            bgPosX = bgPosY = 0;
            updateBgStyle();
          }

          function onwheel(e) {
            var deltaY = 0;

            e.preventDefault();

            if (e.deltaY) { // FireFox 17+ (IE9+, Chrome 31+?)
              deltaY = e.deltaY;
            } else if (e.wheelDelta) {
              deltaY = -e.wheelDelta;
            }

            // As far as I know, there is no good cross-browser way to get the cursor position relative to the event target.
            // We have to calculate the target element's position relative to the document, and subtrack that from the
			// cursor's position relative to the document.
            var rect = img.getBoundingClientRect();
            var offsetX = e.pageX - rect.left - window.pageXOffset;
            var offsetY = e.pageY - rect.top - window.pageYOffset;

            // Record the offset between the bg edge and cursor:
              var bgCursorX = offsetX - bgPosX;
            var bgCursorY = offsetY - bgPosY;

            // Use the previous offset to get the percent offset between the bg edge and cursor:
              var bgRatioX = bgCursorX/bgWidth;
            var bgRatioY = bgCursorY/bgHeight;

            // Update the bg size:
              if (deltaY < 0) {
                bgWidth += bgWidth*settings.zoom;
                bgHeight += bgHeight*settings.zoom;
              } else {
                bgWidth -= bgWidth*settings.zoom;
                bgHeight -= bgHeight*settings.zoom;
              }

            if (settings.maxZoom) {
              bgWidth = Math.min(width*settings.maxZoom, bgWidth);
              bgHeight = Math.min(height*settings.maxZoom, bgHeight);
            }

            // Take the percent offset and apply it to the new size:
              bgPosX = offsetX - (bgWidth * bgRatioX);
            bgPosY = offsetY - (bgHeight * bgRatioY);

            // Prevent zooming out beyond the starting size
            if (bgWidth <= width || bgHeight <= height) {
              reset();
            } else {
              updateBgStyle();
            }
          }

          function drag(e) {
            e.preventDefault();
            bgPosX += (e.pageX - previousEvent.pageX);
            bgPosY += (e.pageY - previousEvent.pageY);
            previousEvent = e;
            updateBgStyle();
          }

          function removeDrag() {
            document.removeEventListener('mouseup', removeDrag);
            document.removeEventListener('mousemove', drag);
          }

          // Make the background draggable
          function draggable(e) {
            e.preventDefault();
            previousEvent = e;
            document.addEventListener('mousemove', drag);
            document.addEventListener('mouseup', removeDrag);
          }

          function load() {
            var initial = Math.max(settings.initialZoom, 1);

            if (img.src === transparentSpaceFiller) return;

            var computedStyle = window.getComputedStyle(img, null);

            width = parseInt(computedStyle.width, 10);
            height = parseInt(computedStyle.height, 10);
            bgWidth = width * initial;
            bgHeight = height * initial;
            bgPosX = -(bgWidth - width) * settings.initialX;
            bgPosY = -(bgHeight - height) * settings.initialY;

            setSrcToBackground(img);

            img.style.backgroundSize = bgWidth+'px '+bgHeight+'px';
            img.style.backgroundPosition = bgPosX+'px '+bgPosY+'px';
            img.addEventListener('wheelzoom.reset', reset);

            img.addEventListener('wheel', onwheel);
            img.addEventListener('mousedown', draggable);
          }

          var destroy = function (originalProperties) {
            img.removeEventListener('wheelzoom.destroy', destroy);
            img.removeEventListener('wheelzoom.reset', reset);
            img.removeEventListener('load', load);
            img.removeEventListener('mouseup', removeDrag);
            img.removeEventListener('mousemove', drag);
            img.removeEventListener('mousedown', draggable);
            img.removeEventListener('wheel', onwheel);

            img.style.backgroundImage = originalProperties.backgroundImage;
            img.style.backgroundRepeat = originalProperties.backgroundRepeat;
            img.src = originalProperties.src;
          }.bind(null, {
            backgroundImage: img.style.backgroundImage,
            backgroundRepeat: img.style.backgroundRepeat,
            src: img.src
          });

          img.addEventListener('wheelzoom.destroy', destroy);

          options = options || {};

          Object.keys(defaults).forEach(function(key){
            settings[key] = options[key] !== undefined ? options[key] : defaults[key];
          });

          if (img.complete) {
            load();
          }

          img.addEventListener('load', load);
        };

        // Do nothing in IE9 or below
        if (typeof window.btoa !== 'function') {
          return function(elements) {
            return elements;
          };
        } else {
          return function(elements, options) {
            if (elements && elements.length) {
              Array.prototype.forEach.call(elements, main, options);
            } else if (elements && elements.nodeName) {
              main(elements, options);
            }
            return elements;
          };
        }
      }());",
    "</script>",
    "<script>",
    "wheelzoom(document.querySelector('img.zoom'));",
    "</script>")


  html_page <- c(
    "<html>",
    "<head>",
    "<script>",
    "window.onresize = function(){ window.setTimeout(function(){window.location.reload();}, 100)}",
    "</script>",
    "</head>",
    "<body style='height:100%;width:100%;margin:auto;'>",
    viewer_html,
    "</body>",
    "</html>"
  )

  cat(html_page,
      sep = "\n",
      file = filename)
}
