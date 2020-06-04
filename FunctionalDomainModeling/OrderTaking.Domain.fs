namespace OrderTaking.Domain

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols
open FSharpx.Collections

type Undefined = exn

// Product code related
type WidgetCode = WidgetCode of string
 // constraint: starting with "W" then 4 digits
type GizmoCode = GizmoCode of string
 // constraint: starting with "G" then 3 digits
type ProductCode =
    | Widget of WidgetCode
    | Gizmo of GizmoCode

// Order quantity related
type UnitQuantity = private UnitQuantity of int
 // between 1 and 1000
type KilogramQuantity = private KilogramQuantity of decimal<kg>
 // between 0.05 and 100.00
type OrderQuantity =
    | Unit of UnitQuantity
    | Kilogram of KilogramQuantity

type OrderId = Undefined
type OrderLineId = Undefined
type CustomerId = Undefined

type CustomerInfo = Undefined
type ShippingAddress = Undefined
type BillingAddress = Undefined
type Price = Undefined
type BillingAmount = Undefined

type Order = {
    Id : OrderId // id for entity
    CustomerId : CustomerId // customer reference
    ShippingAddress : ShippingAddress
    BillingAddress : BillingAddress
    OrderLines : NonEmptyList<OrderLine>
    AmountToBill : BillingAmount }

and OrderLine = {
    Id : OrderLineId // id for entity
    OrderId : OrderId // order reference
    ProductCode : ProductCode
    OrderQuantity : OrderQuantity
    Price : Price }

type UnvalidatedOrder = {
    OrderId : string
    CustomerInfo : Undefined
    ShippingAddress : Undefined }

type PlaceOrderEvents = {
    AcknowledgementSent : Undefined
    OrderPlaced : Undefined
    BillableOrderPlaced : Undefined }

type PlaceOrderError =
    | ValidationError of ValidationError list

and ValidationError = {
    FieldName : string
    ErrorDescription : string }

type PlaceOrder =
    UnvalidatedOrder -> Result<PlaceOrderEvents,PlaceOrderError>

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
