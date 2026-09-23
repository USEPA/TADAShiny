### Pull Request Checklist (convert PR to draft if in progress)

-   [ ] Update your branch from the latest `develop` and resolve any merge conflicts

-   [ ] Run devtools::test(), devtools::check(), and devtools::document() locally; ensure tests pass and fix any errors, warnings, or notes. Add new dependencies to `DESCRIPTION` and document appropriately

-   [ ] Request review from at least one developer team member (convert PR to ready for review if it was designated as in progress)

-   [ ] Include a summary of the changes made and relevant context/motivation

-   [ ] Link issues to auto-close on merge (use Development sidebar or include "Closes #<issue-number>" in the PR)

-   [ ] Refresh inline/block comments for clarity

-   [ ] Review the bot's coverage report and add/update tests in `tests/testthat`

-   [ ] If there is a bot spelling comment, run spelling::spell_check_package() locally and fix any misspellings; add approved project terms to WORDLIST with spelling::update_wordlist()
