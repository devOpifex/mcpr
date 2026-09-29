skip_if_not_installed("ellmer", "0.3.0")

fake_tools <- list(
  list(
    name = "add",
    description = "Add two numbers",
    inputSchema = list(
      type = "object",
      properties = list(
        a = list(type = "number", description = "first"),
        b = list(type = "number", description = "second")
      ),
      required = list("a")
    )
  ),
  list(
    name = "ping",
    description = "No arguments",
    inputSchema = list(type = "object", properties = list())
  )
)

mock_client <- function(env = parent.frame()) {
  calls <- new.env()
  local_mocked_bindings(
    tools_list = function(mcp) list(result = list(tools = fake_tools)),
    tools_call = function(mcp, params, id = NULL) {
      calls$last <- params
      list(result = list(content = list(list(type = "text", text = "ok"))))
    },
    .env = env
  )
  calls
}

test_that("basic types map and follow schema$required", {
  types <- create_ellmer_types(list(
    properties = list(
      s = list(type = "string"),
      n = list(type = "number"),
      i = list(type = "integer"),
      b = list(type = "boolean")
    ),
    required = list("s", "i")
  ))

  expect_equal(
    vapply(types, function(x) x@type, character(1)),
    c(s = "string", n = "number", i = "integer", b = "boolean")
  )
  expect_equal(
    vapply(types, function(x) x@required, logical(1)),
    c(s = TRUE, n = FALSE, i = TRUE, b = FALSE)
  )
})

test_that("enums are detected from the enum field", {
  type <- create_ellmer_type(list(type = "string", enum = list("a", "b")))
  expect_s7_class(type, ellmer::TypeEnum)
  expect_equal(type@values, c("a", "b"))
})

test_that("arrays carry their item type", {
  type <- create_ellmer_type(list(
    type = "array",
    description = "arr",
    items = list(type = "string")
  ))
  expect_s7_class(type, ellmer::TypeArray)
  expect_equal(type@description, "arr")
  expect_equal(type@items@type, "string")
})

test_that("nested objects carry their properties", {
  type <- create_ellmer_type(list(
    type = "object",
    description = "obj",
    properties = list(x = list(type = "string")),
    required = list("x")
  ))
  expect_s7_class(type, ellmer::TypeObject)
  expect_named(type@properties, "x")
  expect_true(type@properties$x@required)
})

test_that("nullable and missing types fall back gracefully", {
  nullable <- create_ellmer_type(list(type = list("string", "null")))
  expect_equal(nullable@type, "string")
  expect_false(nullable@required)

  untyped <- create_ellmer_type(list(anyOf = list(list(type = "string"))))
  expect_equal(untyped@type, "string")
})

test_that("mcpr_to_ellmer_tools builds tools without warnings", {
  mock_client()
  expect_no_warning(tools <- mcpr_to_ellmer_tools(structure(list(), class = "client")))
  expect_named(tools, c("add", "ping"))
  expect_s3_class(tools$add, "ellmer::ToolDef")
  expect_equal(tools$add@name, "add")
})

test_that("handler omits optional arguments that were not given", {
  calls <- mock_client()
  tools <- mcpr_to_ellmer_tools(structure(list(), class = "client"))

  tools$add(a = 1)
  expect_equal(calls$last$name, "add")
  expect_equal(calls$last$arguments, list(a = 1))

  tools$add(a = 1, b = 2)
  expect_equal(calls$last$arguments, list(a = 1, b = 2))
})

test_that("tools without parameters send an empty object", {
  calls <- mock_client()
  tools <- mcpr_to_ellmer_tools(structure(list(), class = "client"))

  tools$ping()
  expect_equal(to_json(calls$last), '{"name":"ping","arguments":{}}')
})

test_that("register_mcpr_tools registers tools on a chat", {
  mock_client()
  chat <- ellmer::chat_openai(model = "gpt-4o", credentials = function() "x")
  register_mcpr_tools(chat, structure(list(), class = "client"))
  expect_named(chat$get_tools(), c("add", "ping"))
})
