require "rubygems"
require "watir-webdriver"

Given /^that I have gone to the page$/ do
  # @browser = Wait::Chrome.new
  # @browser.goto("index.html")
  pending
end

When /^I click \((\d+),(\d+)\) in the grid$/ do |arg1, arg2|
  pending # express the regexp above with the code you wish you had
end

Then /^it toggles colour$/ do
  pending # express the regexp above with the code you wish you had
end

When /^click \((\d+),(\d+)\) in the grid$/ do |arg1, arg2|
  pending # express the regexp above with the code you wish you had
end

Then /^the colour is maintained$/ do
  pending # express the regexp above with the code you wish you had
end
