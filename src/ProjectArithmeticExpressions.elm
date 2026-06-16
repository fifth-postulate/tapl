module ProjectArithmeticExpressions exposing (main)

import General
import GenericPage
import Language.ArithmeticExpressions as Expression


main =
    GenericPage.create
        { title = "Arithmetic Expression"
        , parse = Expression.parse
        , printer = Expression.toString
        , evaluator = General.eval Expression.step
        }
