app <- ShinyDriver$new("../")
app$snapshotInit("app_gallery")

# Input '`plotly_hover-A`' was set, but doesn't have an input binding.
# Input '`plotly_afterplot-A`' was set, but doesn't have an input binding.
# Input '`plotly_relayout-A`' was set, but doesn't have an input binding.
app$snapshot()
