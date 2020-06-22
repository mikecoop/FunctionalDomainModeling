namespace OrderTaking.Domain

open SimpleTypes
open FSharpx.Collections

type Undefined = exn

type CustomerId = Undefined

type PersonalName = {
    FirstName : String50
    LastName : String50 }

type CustomerInfo = {
    Name : PersonalName
    EmailAddress : EmailAddress }

type UnvalidatedCustomerInfo = {
    FirstName : string
    LastName : string
    EmailAddress : string }

type Address = {
    AddressLine1 : String50
    AddressLine2 : String50 option
    AddressLine3 : String50 option
    AddressLine4 : String50 option
    City : String50
    ZipCode : ZipCode }

type ShippingAddress = Address
type BillingAddress = Address
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
    Quantity : OrderQuantity
    Price : Price }

type PricedOrderLine = {
    OrderLineId : OrderLineId
    ProductCode : ProductCode
    Quantity : OrderQuantity
    LinePrice : Price }

type UnvalidatedAddress = {
    AddressLine1 : string
    AddressLine2 : string
    AddressLine3 : string
    AddressLine4 : string
    City : string
    ZipCode : string }

type CheckedAddress = CheckedAddress of UnvalidatedAddress

type CheckProductCodeExists = ProductCode -> bool

type UnvalidatedOrderLine = {
    OrderLineId : int
    ProductCode : string
    Quantity : decimal }

type UnvalidatedOrder = {
    OrderId : string
    CustomerInfo : UnvalidatedCustomerInfo
    ShippingAddress : UnvalidatedAddress
    BillingAddress : UnvalidatedAddress
    OrderLines : UnvalidatedOrderLine list }

type CheckAddressExists =
    UnvalidatedAddress -> CheckedAddress

type ValidatedOrderLine = {
    OrderLineId : OrderLineId
    ProductCode : ProductCode
    Quantity : OrderQuantity }

type ValidatedOrder = {
    OrderId : OrderId
    CustomerInfo : CustomerInfo
    ShippingAddress : Address
    BillingAddress : Address
    Lines : ValidatedOrderLine list }

type PricedOrder = {
    OrderId : OrderId
    CustomerInfo : CustomerInfo
    ShippingAddress : ShippingAddress
    BillingAddress : BillingAddress
    Lines : PricedOrderLine list
    AmountToBill : BillingAmount }

type GetProductPrice = ProductCode -> Price

type PriceOrder = GetProductPrice -> ValidatedOrder -> PricedOrder

type HtmlString = HtmlString of string
type CreateOrderAcknowledgementLetter = PricedOrder -> HtmlString

type OrderAcknowledgement = {
    EmailAddress : EmailAddress
    Letter : HtmlString }

type SendResult = Sent | NotSent
type SendOrderAcknowledgement = OrderAcknowledgement -> SendResult

type OrderAcknowledgementSent = {
    OrderId : OrderId
    EmailAddress : EmailAddress }

type AcknowledgeOrder =
    CreateOrderAcknowledgementLetter
     -> SendOrderAcknowledgement
     -> PricedOrder
     -> OrderAcknowledgementSent option

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

type OrderPlaced = PricedOrder

type BillableOrderPlaced = {
    OrderId : OrderId
    BillingAddress : Address
    AmountToBill : BillingAmount }

type PlaceOrderEvent =
    | OrderPlaced of OrderPlaced
    | BillableOrderPlaced of BillableOrderPlaced
    | AcknowledgementSent of OrderAcknowledgementSent

type CreateEvents =
    PricedOrder
        -> OrderAcknowledgementSent option
        -> PlaceOrderEvent list

type PlaceOrderWorkflow =
    UnvalidatedOrder
     -> PlaceOrderEvent list
