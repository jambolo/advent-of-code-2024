# Advent Of Code 2024
Advent of Code 2024 solutions in Haskell. The development environment is VSCode and stack in Windows.

### Day 1

In general terms, the solution for both parts of day 1 is trivial, but wow Haskell has a *steep* learning curve. Github Copilot is a savior.

### Day 2

Part 1 was trivial again and the biggest difficulty was dealing with Haskell. I tried to implement part 2 using recursion, but I couldn't get it to work. I gave up and implemented a more brute force O(n<sup>2</sup>) solution that I feel is not in the spirit of Haskell. I'm curious to see a real Haskell solution.

### Day 3

Trivial again. Luckily, Haskell has a regex library and the solution wasn't too hard to figure out once I figured out how to use the regex library.

### Day 4
Trivial. I added a little cleverness, but otherwise I just did it in a straightforward way. I'm glad Haskell has some comprehension and range support.

### Day 5
Simple enough. Haskell seems to have a decent collection of containers.

### Day 6
I started to develop an optimized solution because I was afraid that part 2 would require it. It was taking a long time to implement, so I just implemented the simple solution and that turned out to be ok.

My initial solution for part 2 was too low. I did not put an obstacle at the starting point because that didn't seem to make sense. But, when I included an obstacle at the starting point, I got the correct answer. I'm not happy about that.

Haskell insisted that I change a complex (though straightforward) if-then-else structure returning Bool to an equivalent complicated logical expression. I prefer the if-then-else because it is much easier to read, but Haskell kept insisting so I acquiesced.

### Day 7
Super easy. I learned a little more about comprehensions, specifically this trickery: `nextPowerOf10 n = head [10^x | x <- [1..], 10^x > n]`

### Day 8
I encountered what I feel is problem with Haskell. It seems wrong to me that `head [10^x | x <- [1..], 10^x > n]` terminates, when `[10^x | x <- [1..], 10^x > n]` does not.

### Day 9
Performance is going to be critical for this problem. I found that building a 94422 element list by *appending* elements takes 16000 ms, but building the list by *prepending* elements and then reversing takes 6 ms.

Part 1 took about 50 seconds to execute. Surprisingly, part 2 only took 15 seconds. I think it was because I was able to organize the sectors into buckets in part 2, making the size of the list 65%-80% smaller.

### Day 10
Pretty easy again. I think Haskell is suited for this kind of recursive problem.

### Day 11
Part 2 has a clever solution. 

### Day 12
Pretty easy, though some minor errors were hard to find.

### Day 13
Number theory is not my strength, but luckily I suspected that there is a known solution to this kind of problem and it turns out that there is. Unfortunately, it is not fast enough for part 2.

### Day 14


### Day 15
Part 1 was pretty easy. I am getting more comfortable with Haskell's ways of doing iteration using recursion.

### Day 16
Simple path-finding with a twist. I ended up using someone else's A* library because implementing A* is always a non-trivial task and implementing it in Haskell would take a very long time. Unfortunately, that won't work for part 2 because I need to return *all possible* paths of with the shortest length. I'm starting to like Haskell less because the code is necessarily very complex and debugging is very cumbersome.

### Day 17
Implementing a virtual CPU was fun but tedious. Luckily the problem was designed in a way that Haskell could handle without too much trouble. Part 1 was relatively easy. Part 2 required a clever solution that I found after looking at the program being executed.

### Day 18
Really trivial path-finding. I don't know why they included this one *after* Day 16 which was a more complex path-finding problem.

### Day 19
Implementing part 1 and part 2 were trivial. Haskell is designed for this kind of problem -- a recursive search. Unfortunately, part 2 is too slow to find a solution.

### Day 20
Not much to say.




### Day 22
Lots of stuff to track and manipulate, but otherwise not too difficult. However, part 2 is too slow to get a solution. I was hoping that analyzing the PRNG might give clues to some potential cleverness, there is nothing helpful there.

### Day 23
A relatively simple graph theory problem. One of the issues I have with learning Haskell is that the errors messages are concise, but they are not clear (due primarily to my inexperience). Thankfully, Copilot excels at explaining the cause of an error and suggesting a solution.

