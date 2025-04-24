---
name: Bug report
about: Create a report to help us improve
title: ''
labels: ''
assignees: ''
editor_options: 
  markdown: 
    wrap: 72
---

**Describe the bug**

A clear and concise description of what the bug is.

**To Reproduce**

Steps to reproduce the behavior:

1\. Go to '...'

2\. Click on '....'

3\. Scroll down to '....'

4\. See error

Please include your Water Quality Portal data query inputs from the Load
data tab:

If applicable, include code to reproduce the behavior:

``` r
library(TADAShiny)
TADAShiny::run_app()
```

**Expected behavior**

A clear and concise description of what you expected to happen.

**Screenshots**

If applicable, add screenshots to help explain your problem.

**Additional context**

Add any other context about the problem here.

**Reminders for TADA contributors addressing this issue**

Bug fixes should include the following work:

-   [ ] Create or edit the code.

-   [ ] Document all code using line/inline and/or multi-line/block comments
    to describe what is does.

-   [ ] Create or edit tests in tests/testthat folder to help prevent and/or 
    troubleshoot potential future issues.

-   [ ] If your code edits impact other functionality in the shiny 
    app, ensure those are updated as well.

-   [ ] Run styler::style_pkg(), devtools::document(), and devtools::check() 
    and address any new notes or issues before creating a pull request.
