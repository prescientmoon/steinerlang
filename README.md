# Steiner

Lang I'm making for fun

## How to run locally

You first need to have purescript and spago installed locally.

1. Clone the repo
2. Run `spago run`

This will start the steiner repl.

> Note: the first time you try and run it will take a while since spago will have to install all the dependencies and stuff.

## Commands in the repl

| Command            | Description                                           |
| ------------------ | ----------------------------------------------------- |
| :ast expr          | print the ast an expression parses to                 |
| :t expr            | Infer the type of an expression                       |
| :check (expr) type | Check if an expression has a type                     |
| :s type type       | Check if a type is at least as polymorphic as another |
| :u type type       | See the result of unifying 2 types                    |
| :q                 | Quit the repl                                         |
| :clear             | Clear the console                                     |

## Syntax

### Comments

1. single line


    ```haskell
    -- something
    ```

2. multi line


    ```haskell
    {-
        Something
    -}
    ```

### Types

- Constants:

  Constants can start with any uppercase letter

  ```haskell
  Int
  String
  Bool
  ...
  ```

- Variables:

  Variables need to start with a lowercase letter

  ```haskell
  something
  otherVar
  ...
  ```

- Lambdas

  ```haskell
  type -> type
  ```

- Rank N Types
  ```haskell
  forall name. type -- this can reference name
  ```

### Expressions

- Int literals

  ```haskell
  0
  ```

- float literals

  ```haskell
  0.0
  ```

- string literals

  ```haskell
  "something"
  ```

- If expressions

  ```haskell
  if condition then expression else expression
  ```

- Lambdas
  ```haskell
  \a -> expression -- this can reference a
  ```
- Type annotations

  ```haskell
  expression :: type
  ```

- Application

  ```haskell
  function argument
  ```

- Let expressions

  ```haskell
  let name = expression in body -- this can reference name
  ```

- Assumptions
  ```haskell
  assume name :: type in expression -- this can reference name
  ```
