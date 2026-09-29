# Create a response object

Create a response object

## Usage

``` r
response_text(text)

response_image(image, mime_type = "image/png")

response_audio(audio, mime_type = "audio/mpeg")

response_video(video, mime_type = "video/mp4")

response_file(file, mime_type = "application/octet-stream")

response_resource(resource)

response_error(text)

response_item(
  ...,
  type = c("text", "image", "audio", "video", "file", "resource")
)

response(..., is_error = FALSE)
```

## Arguments

- text:

  Text content for the response

- image:

  Image content

- mime_type:

  Mime type of the content

- audio:

  Audio content

- video:

  Video content

- file:

  File content

- resource:

  Resource content

- ...:

  Mutliple `response` objects or passed to
  [`ggplot2::ggsave`](https://ggplot2.tidyverse.org/reference/ggsave.html)

- type:

  Type of the content

- is_error:

  Whether the response is an error

## Value

A response object

## Details

Use `response_item` to create a custom response item.

## Examples

``` r
response(
  response_text("Hello, world!"),
  response_image(system.file("extdata/logo.png", package = "mcpr")),
  response_audio(system.file("extdata/sound.mp3", package = "mcpr")),
  response_video(system.file("extdata/video.mp4", package = "mcpr")),
  response_file(system.file("extdata/file.txt", package = "mcpr")),
  response_resource(system.file("extdata/resource.json", package = "mcpr"))
)
#> $content
#> $content[[1]]
#> $type
#> [1] "text"
#> 
#> $text
#> [1] "Hello, world!"
#> 
#> attr(,"class")
#> [1] "response_item"      "response_item_text" "list"              
#> 
#> $content[[2]]
#> $type
#> [1] "image"
#> 
#> $data
#> [1] ""
#> 
#> $mimeType
#> [1] "image/png"
#> 
#> attr(,"class")
#> [1] "response_item"       "response_item_image" "list"               
#> 
#> $content[[3]]
#> $type
#> [1] "audio"
#> 
#> $data
#> [1] ""
#> 
#> $mimeType
#> [1] "audio/mpeg"
#> 
#> attr(,"class")
#> [1] "response_item"       "response_item_audio" "list"               
#> 
#> $content[[4]]
#> $type
#> [1] "video"
#> 
#> $data
#> [1] ""
#> 
#> $mimeType
#> [1] "video/mp4"
#> 
#> attr(,"class")
#> [1] "response_item"       "response_item_video" "list"               
#> 
#> $content[[5]]
#> $type
#> [1] "file"
#> 
#> $data
#> [1] ""
#> 
#> $mimeType
#> [1] "application/octet-stream"
#> 
#> attr(,"class")
#> [1] "response_item"      "response_item_file" "list"              
#> 
#> $content[[6]]
#> $type
#> [1] "resource"
#> 
#> $resource
#> [1] ""
#> 
#> attr(,"class")
#> [1] "response_item"          "response_item_resource" "list"                  
#> 
#> 
#> $isError
#> [1] FALSE
#> 
#> attr(,"class")
#> [1] "response" "list"    
```
