module PlaceOrderWorkflow

open SimpleTypes
open FSharpx.Collections

type Undefined = exn

// ----------- Validate Order -----------

type CheckProductCodeExists = ProductCode -> bool

type UnvalidatedAddress = {
    AddressLine1 : string
    AddressLine2 : string
    AddressLine3 : string
    AddressLine4 : string
    City : string
    ZipCode : string }

type CheckedAddress = CheckedAddress of UnvalidatedAddress

type CheckAddressExists =
    UnvalidatedAddress -> CheckedAddress

type UnvalidatedOrderLine = {
    OrderLineId : int
    ProductCode : string
    Quantity : decimal }

type UnvalidatedCustomerInfo = {
    FirstName : string
    LastName : string
    EmailAddress : string }

type UnvalidatedOrder = {
    OrderId : string
    CustomerInfo : UnvalidatedCustomerInfo
    ShippingAddress : UnvalidatedAddress
    BillingAddress : UnvalidatedAddress
    OrderLines : UnvalidatedOrderLine list }

type ValidatedOrderLine = {
    OrderLineId : OrderLineId
    ProductCode : ProductCode
    Quantity : OrderQuantity }

type PersonalName = {
    FirstName : String50
    LastName : String50 }

type CustomerInfo = {
    Name : PersonalName
    EmailAddress : EmailAddress }

type Address = {
    AddressLine1 : String50
    AddressLine2 : String50 option
    AddressLine3 : String50 option
    AddressLine4 : String50 option
    City : String50
    ZipCode : ZipCode }

type ValidatedOrder = {
    OrderId : OrderId
    CustomerInfo : CustomerInfo
    ShippingAddress : Address
    BillingAddress : Address
    Lines : ValidatedOrderLine list }

type ValidateOrder =
    CheckProductCodeExists
     -> CheckAddressExists
     -> UnvalidatedOrder
     -> ValidatedOrder

// ----------- Pricing Order -----------

type GetProductPrice = ProductCode -> Price

type ShippingAddress = Address

type BillingAddress = Address

type PricedOrderLine = {
    OrderLineId : OrderLineId
    ProductCode : ProductCode
    Quantity : OrderQuantity
    LinePrice : Price }

type PricedOrder = {
    OrderId : OrderId
    CustomerInfo : CustomerInfo
    ShippingAddress : ShippingAddress
    BillingAddress : BillingAddress
    Lines : PricedOrderLine list
    AmountToBill : BillingAmount }

type PriceOrder = GetProductPrice -> ValidatedOrder -> PricedOrder

// ----------- Send Order Acknowledgement -----------

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

// ----------- Create Events -----------

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

// ----------- Place Order Workflow -----------

type PlaceOrderWorkflow =
    UnvalidatedOrder
     -> PlaceOrderEvent list