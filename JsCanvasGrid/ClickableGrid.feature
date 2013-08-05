Feature: Grid Toggle
  In order to toggle the grid
  I need to be able to click on cells

Scenario:
  Given that I have gone to the page
  When I click (10,10) in the grid
  Then it toggles colour

Scenario:
  Given that I have gone to the page
  When I click (10,10) in the grid
  And click (10,10) in the grid
  Then the colour is maintained