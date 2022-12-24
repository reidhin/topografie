// Taken from: https://www.reddit.com/r/rstats/comments/7fmkah/moving_focus_to_next_input_in_shiny/
// refocus on certain element
Shiny.addCustomMessageHandler(
  "refocus",
  function(e_id) {
    document.getElementById(e_id).focus();
  }
);

