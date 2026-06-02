module ProjectUntypedLambdaCalculus exposing (main)

import General
import GenericPage
import UntypedLambdaCalculus as Expression exposing (empty)


main =
    GenericPage.create
        { title = "Untyped Lambda Calculus"
        , parse = Expression.parse
        , printer = Expression.toString empty
        , evaluator = General.eval (Expression.step empty)
        }
