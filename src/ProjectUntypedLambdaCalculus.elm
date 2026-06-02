module ProjectUntypedLambdaCalculus exposing (main)

import General
import GenericPage
import UntypedLambdaCalculus as Expression exposing (default, empty)


main =
    GenericPage.create
        { title = "Untyped Lambda Calculus"
        , parse = Expression.parse default
        , printer = Expression.toString empty
        , evaluator = General.eval (Expression.step empty)
        }
