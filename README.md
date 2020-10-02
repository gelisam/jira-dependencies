Type this in your terminal: imgcat doesn't work from a script for some reason

    stack run jira-dependencies in-range-answers.csv | dot -Tpng | imgcat

Should output a png illustrating the dependencies between all the issues
exported from JIRA.
