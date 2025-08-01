test_that("can convert basic types to json schema", {
  expect_equal(
    as_json(test_provider(), type_boolean("desc")),
    list(type = "boolean", description = "desc")
  )

  expect_equal(
    as_json(test_provider(), type_enum(letters[1:3], "desc")),
    list(type = "string", description = "desc", enum = as.list(letters[1:3]))
  )

  expect_equal(
    as_json(test_provider(), type_array(type_boolean("b"), "a")),
    list(
      type = "array",
      description = "a",
      items = list(type = "boolean", description = "b")
    )
  )
})

test_that("can convert an object to json schema", {
  obj <- type_object(
    "a",
    integer = type_integer(),
    number = type_number(),
    string = type_string()
  )

  expect_equal(
    as_json(test_provider(), obj),
    list(
      type = "object",
      description = "a",
      properties = list(
        integer = list(type = "integer", description = ""),
        number = list(type = "number", description = ""),
        string = list(type = "string", description = "")
      ),
      required = list("integer", "number", "string"),
      additionalProperties = FALSE
    )
  )
})
