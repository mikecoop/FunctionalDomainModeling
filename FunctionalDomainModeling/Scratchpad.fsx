#load "OrderTaking.Domain.fs"

open OrderTaking.Domain

let unitQtyResult = UnitQuantity.create 5

match unitQtyResult with
| Error msg ->
    printfn "Failure, Message is %s" msg
| Ok qty ->
    printfn "Success. Value is %A" qty
    let innerValue = UnitQuantity.value qty
    printfn "innerValue is %i" innerValue
