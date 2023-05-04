# Baesich
Baesich is a multibase calculator with some funky extra features in the form of "naive" operations, where numbers are added, subtracted, multiplied, (integer) divided, or converted without regard to their actual value and instead by their current integer representation.



## Notes/Examples
Baesich runs commands like a normal calculator would (8 + 10, 3*(6/5), etc.) using the four 'primary' operations, along with an additional operator symbol '|' which allows one to convert a number or expression into another base (ex. 4 | 2 becomes 100).

For working with other bases, the notation is b(#)(#), where the '#' represent numbers or expressions. All numbers are initially treated as though you are writing them in decimal (although in our hypothetical future plans we would like that to be a setting easily changed via a command), which is to say that b(2)(10) represents the binary value of 1010. These are fully recognized as expressions, meaning that they can be combined and expanded with other operations seamlessly.

Finally, we have our strangest implementation: naivety. A base conversion of b(2)(10) equals 1010, but if we write it as n(2)(10), it equals 10. The useful symbols '<' and '>' indicate a naive operation is in play, and tells you the base to use, i.e. b(2)(4) <* n(5)(24) will be run in base 2. '<>' around an operator will cause the base to be added or subtracted as well relative to the operation (with the caveat that subtracting bases results in an absolute value base).

Have fun experimenting :)



## Contributions
Jauss: I did a lot of the typing and translation of solutions into Haskell. Garrett insists I did a majority of the work but I can't claim that. I spent a lot of time circling on specific rules, and had I been alone, I would have gone down far too many blind alleys without Garrett's presence. I did a lot of development on concepts and features that couldn't be implemented on time. 

Garrett: Though I don't have many commits on the github, this is because we spent our time working together and did our coding using Jauss's laptop as I lack a laptop to do coding on the go. I did my share of the bug checking and implementation, but what I think the most important contribution both of us applied was in being a measure of support for the other. We both agree that if this had been a single person project it would've been far far more difficult to complete, especially by the due date, as so much more time would've been spent simply deer in the headlights staring at the screen when something goes wrong or a question comes up of how exactly to implement a feature when the first idea won't work.