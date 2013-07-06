knox
====

Demo shiny app

Many thanks to the shiny dev team. Shiny is sooooo cool!

Known issue:

1. On the Plot tab of the siderbar, when you click the textbox and input "2 * x1 + 1", then click Plot button.
   The app make some plots, and is supposed to add "2*x1+1" to all four select inputs on both the Plot tab and the
   Model tab. However, only the two select inputs on the PLot tab are affected. Search for PROBLEM in server.R to
   locate the code that updates the select inputs.


Main features:

1. "+" tab on main panel that can add a new tab (like a browser).

2. Message handling to allow UI changes (header logo changes) when heavy computation is still running.

3. googleVis plots (when you click Plot).

4. Dynamically inserts png plots into a tab in main panel (when you click Run on the model tab, try click it multiple
   times).

5. Automatic switching between tabs (click the Plot button on the siderbar, main panel switches to Plot tab; click
   the Run button on the siderbar, main panel will create a Model1 tab).


Minor features:

1. Customized header panel with image.

2. Insert css and javascript.

3. Help Popup icon (https://gist.github.com/jcheng5/5913297).


Things I hope to have:

1. rCharts. This one is quite hard. Main difficulty is the lack of documentation.

