require "rubygems"
require "watir"

Given /^that I have gone to the page$/ do
  @browser = Wait::Chrome.new
  @browser.goto("index.html")
end

