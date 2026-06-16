module ProjectUntypedLambdaCalculus exposing (main)

import General
import GenericPage
import Language.UntypedLambdaCalculus as Expression exposing (default)


main =
    GenericPage.create
        { title = "Untyped Lambda Calculus"
        , parse = Expression.parse default
        , printer = Expression.toString default
        , evaluator = General.eval (Expression.step default)
        }
