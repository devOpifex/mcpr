# Create a new prompt

Create a new prompt

## Usage

``` r
new_prompt(name, description, arguments = list(), handler)
```

## Arguments

- name:

  Name of the prompt

- description:

  Description of the prompt

- arguments:

  List of arguments for the prompt

- handler:

  Function to handle the prompt execution

## Value

A new prompt capability

## Examples

``` r
prompt <- new_prompt(
  name = "My Prompt",
  description = "This is a description",
  arguments = list(
    input1 = list(
      type = "string",
      description = "Input 1"
    ),
    input2 = list(
      type = "number",
      description = "Input 2"
    )
  ),
  handler = function(params) {
    # Process the prompt request
    return(list(text = "Generated text from prompt"))
  }
)
```
