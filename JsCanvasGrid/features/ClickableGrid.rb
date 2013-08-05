require "rubygems"
require "watir-webdriver"


Given /^that I have started the server$/ do
  # I'd use this to start the server
end

Given /^that I have gone to the page$/ do
  @browser = Watir::Browser.new
  @browser.goto 'http://localhost:8000/index.html'
end

When /^I click \((\d+),(\d+)\) in the grid$/ do |arg1, arg2|
  pending
end

Then /^it toggles colour$/ do
  pending
end

When /^click \((\d+),(\d+)\) in the grid$/ do |arg1, arg2|
  pending
end

Then /^the colour is maintained$/ do
  pending
end

After do |scenario|
  @browser.close
end
