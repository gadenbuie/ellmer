ask_user_for_name <- coro::async(function() {
  session <- shiny::getDefaultReactiveDomain()
  if (is.null(session)) {
    stop("This tool can only be used in a Shiny app.")
  }

  result <- NULL

  obs <- shiny::observeEvent(session$input$tool_username_submit, {
    # shiny::removeModal(session)
    result <<- session$input$tool_username_name
  })

  ui_inline_confirmation <- shiny::div(
    id = "tool_confirmation",
    shiny::textInput("tool_username_name", "Hi! What's your name?"),
    shiny::actionButton("tool_username_submit", "Submit", class = "btn-sm")
  )
  shinychat::chat_append_message(
    "chat",
    list(role = "assistant", content = ui_inline_confirmation)
  )

  timeout <- Sys.time() + 20

  while (is.null(result)) {
    if (Sys.time() > timeout) stop("Timed out waiting for an answer.")
    coro::await(coro::async_sleep(0.25))
  }

  obs$destroy()

  shinychat::chat_append_message(
    "chat",
    list(role = "assistant", content = ""),
    operation = "replace"
  )

  result
})
