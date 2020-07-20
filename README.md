icfpcontest2020 
===============

This is the Project Repository for Team **Hastronauts** submissions for the ICFP Contest 2020
written in **Haskel**

# Content 

The GalaxyEvaluator consists of the Syntax, Parser, Interpreter and Interpreter.Data modules.
It is used by the Test module to generate two images resulting from evaluating the galaxy interaction protocol 
on the empty state and with the vector (0,0).
Send is 


# Further Links
- [Contest Page](https://icfpcontest2020.github.io/)
- [ReadTheDocs](https://message-from-space.readthedocs.io/en/latest/personal-appeal.html)
- [Submission pre-Test System](https://github.com/Skgland/icfp2020-submission-system)


# TODO
- [ ] Implement Send Function (Interpreter.sh)
- [ ] Move everything but Main, Test and Renderer to a library in a lib dir 
    - Renderer can't be part of the library as it uses dependencies not available in the Container during build
      as such it can stay with Test
    - [ ] Move It
    - [ ] Change test in package.yaml to have source-dir lib instead of app, should remove warning about Main module workaround
- [ ] Make Galaxy Evaluater into an interactive Galaxy Pad
  - [ ] how does the galaxy protocol interact with the world, it does not use the send function?
  - [ ] New executable that evaluates galaxy interaction (using evaluator) in a loop
    1. Evaluate from initial input
    2. Display resulting Picture, saving resulting state
    3. Wait for user input (Vector, Coordinate)
    4. Evaluate galaxy protocol from saved state with user input
    5. Continue with second step 
- [ ] Clean up code
- [ ] Improve Documentation
- [ ] Improve and Update Readme
    
# Old Readme Content

- [x] Parser
- [x] Simple Interpreter
- [x] [Interact](https://message-from-space.readthedocs.io/en/latest/condensed-version.html#interact) in `MIB` monad
