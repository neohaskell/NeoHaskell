# Failed experiment attempts

Neither attempt contributes a successful timing sample.

- macOS Cabal baseline36158410770 at5604e6c reached3088 examples,1 failure,67 pending. The existing bad-credentials test failed because the original Homebrew PostgreSQL fixture trusted connections. Fix fixture authentication; preserve assertions.
- Co-location36159986083 at8b586f1 failed before compilation because COLOCATED_REPETITION was required by the helper but missing from the workflow. Fixed jointly in17f9a14; five local orchestration tests pass, including the missing-label regression. Retry36160325157 is running.
- Local runtime-criterion parsing selected0 integration locators. This is not hosted selector execution evidence.
