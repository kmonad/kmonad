# Contributing Changes/Patches to KMonad

Want to contribute to KMonad?  You've come to the right place!  This document
is supposed to help guide you in this very noble quest.

In case you want to make any non-trivial change to KMonad, it's always best to
talk with the community first.  Currently the best way to do that is to create
an issue on GitHub.  There, you can talk through the problems you are having,
as well as the proposed solution.

## Where to Make Pull Requests

  * Pull requests for new features should be made against the `develop` branch.

  * Pull requests for improvements to the documentation, as well as bug fixes,
    of existing features should be made against the `master` branch.

## Making the Change

It's best to create a separate branch in your clone of the [KMonad
repo](https://github.com/kmonad/kmonad/) and then push this branch to
your fork.

Please also **never** merge `master` (or `develop`) into the branch you have
created.  `git` offers the very neat option of rebasing your changes on top of
another branch; use that instead.

Give your commits descriptive names!  Further, it's also highly recommended to
add a description of _why_ you made a certain change.  The syntax for this is
*commit message*, blank line, *more description*.  For example:

  ``` shell
      Commit Message (max. 50 characters)

      Some more useful information of why this change was made; possibly
      how it connects with other commits in this same pr (wrapped at 72
      characters).
  ```

## Opening a pull request

Once you have pushed the branch to your fork, making a pull request is as easy
as visiting that branch; GitHub will then even prompt you for it!

Please include the following information in the description of your pull
request:

- Does your pull request close, or is related to, any existing issues?
- What does your pull request do?  If you add a new feature, an example of how
  you would use it would be most appreciated.
- A brief summary of how your commits fit together to achieve this (if
  necessary).

Please also remember to update the `CHANGELOG.md` file, as well as any other
documentation that your pull request touches upon.  For example, when
implementing a new feature, you should update the `tutorial.kbd` file
accordingly.
