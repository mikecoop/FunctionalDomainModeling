module SimpleTypes

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

type String50 = private String50 of string
module String50 =
    let create str = (String50 str)
    let createOption str = Some (String50 str)

type EmailAddress = private EmailAddress of string
module EmailAddress =
    let create str = (EmailAddress str)

type ZipCode = private ZipCode of string
module ZipCode =
    let create str = (ZipCode str)

type OrderId = private OrderId of string
module OrderId =
    let create str =
        if System.String.IsNullOrEmpty(str) then
            failwith "OrderId must not be null or empty"
        elif str.Length > 50 then
            failwith "OrderId must not be more than 50 chars"
        else
            OrderId str

    let value (OrderId str) = str

type OrderLineId = private OrderLineId of int
module OrderLineId =
    let create int = (OrderLineId int)

// Product code related
type WidgetCode = WidgetCode of string
 // constraint: starting with "W" then 4 digits
type GizmoCode = GizmoCode of string
 // constraint: starting with "G" then 3 digits
type ProductCode =
    | Widget of WidgetCode
    | Gizmo of GizmoCode
module ProductCode =
    let create str = Widget (WidgetCode "abc")

// Order quantity related
type UnitQuantity = private UnitQuantity of int
// between 1 and 1000
module UnitQuantity =
    /// int -> Result<UnitQuantity,string>
    let create qty =
        if qty < 1 then
            Error "UnitQuantity cannot be negative"
        else if qty > 1000 then
            Error "UnitQuantity cannot be more than 1000"
        else
            Ok (UnitQuantity qty)

    let value (UnitQuantity qty) = qty

type KilogramQuantity = private KilogramQuantity of decimal<kg>
// between 0.05 and 100.00
module KilogramQuantity =
    /// decimal -> Result<KilogramQuantity,string>
    let create qty =
        if qty < 0.05m then
            Error "KilogramQuantity cannot be less than 0.05"
        else if qty > 100.0m then
            Error "KilogramQuantity cannot be more than 100.0"
        else
            Ok (KilogramQuantity (qty * 1.0m<kg>))

    let value (KilogramQuantity qty) = qty

type OrderQuantity =
    | Unit of UnitQuantity
    | Kilogram of KilogramQuantity
module OrderQuantity =
    let value = function
    | Unit (UnitQuantity u) -> decimal u
    | Kilogram (KilogramQuantity k) -> decimal k

type Price = Price of decimal
module Price =
    let value (Price p) = p
    let create p = Price p
    let multiply qty (Price p) =
        create (qty * p)

type BillingAmount = BillingAmount of decimal
module BillingAmount =
    let sumPrices prices =
        let total = prices |> List.sumBy Price.value
        BillingAmount total
    let value (BillingAmount amount) = amount
