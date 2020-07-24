ICFP Programming Contest 2020 - Team Hastronauts 
===============

This is the project repository for team **Hastronauts**'s submissions for the ICFP Programming Contest 2020,
written in **Haskell**.

# Content 

The `GalaxyEvaluator` consists of the `Syntax`, `Parser`, `Interpreter` and `Interpreter.Data` modules.
It is used by the `Test` module to generate two images resulting from evaluating the galaxy interaction protocol 
on the empty state and with the vector (0,0).

# Further Links
- [Contest Page](https://icfpcontest2020.github.io/)
- [ReadTheDocs](https://message-from-space.readthedocs.io/en/latest/personal-appeal.html)
- [Submission Pre-Test System](https://github.com/Skgland/icfp2020-submission-system)


# TODO
- [ ] Implement `send` function (Interpreter.sh)
- [ ] Move everything but `Main`, `Test` and `Renderer` to a library in a lib dir 
    - Renderer can't be part of the library as it uses dependencies not available in the container during build,
      as such it can stay with `Test`
    - [ ] Move It
    - [ ] Change test in package.yaml to have source-dir lib instead of app, should remove warning about `Main` module workaround
- [ ] Make galaxy evaluator into an interactive galaxy pad
  - [ ] How does the galaxy protocol interact with the world, it does not use the `send` function?
  - [ ] New executable that evaluates galaxy interaction (using evaluator) in a loop
    1. Evaluate from initial input
    2. Display resulting picture, saving resulting state
    3. Wait for user input (`Vector`, `Coordinate`)
    4. Evaluate galaxy protocol from saved state with user input
    5. Continue with second step 
- [ ] Clean up code
- [ ] Improve documentation
- [ ] Improve and update Readme
