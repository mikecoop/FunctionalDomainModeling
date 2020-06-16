
module Domain

    type Undefined = exn

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

    type String50 = string
    module String50 =
        let create str = str
        let createOption str = str

    type EmailAddress = string
    module EmailAddress =
        let create str = str

    type ZipCode = string
    module ZipCode =
        let create str = str

    type UnvalidatedCustomerInfo = {
        FirstName : string
        LastName : string
        EmailAddress : string }

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
        ZipCode : String50 }

    type UnvalidatedAddress = Undefined
    type CheckedAddress = CheckedAddress of Address
    type CheckProductCodeExists = Undefined

    type UnvalidatedOrder = {
        OrderId : string
        CustomerInfo : UnvalidatedCustomerInfo
        ShippingAddress : Undefined }

    type ValidatedOrder = {
        OrderId : OrderId
        CustomerInfo : CustomerInfo
        ShippingAddress : Address }

    type CheckAddressExists =
        UnvalidatedAddress -> CheckedAddress

    type ValidateOrder =
        CheckProductCodeExists
         -> CheckAddressExists
         -> UnvalidatedOrder
         -> ValidatedOrder

    let toCustomerInfo (customer:UnvalidatedCustomerInfo) : CustomerInfo =
        let firstName = customer.FirstName |> String50.create
        let lastName = customer.LastName |> String50.create
        let emailAddress = customer.EmailAddress |> EmailAddress.create

        let name : PersonalName = {
            FirstName = firstName
            LastName = lastName }

        let customerInfo : CustomerInfo = {
            Name = name
            EmailAddress = emailAddress }

        customerInfo

    let toAddress (checkAddressExists:CheckAddressExists) unvalidatedAddress =
        // call the remote service
        let checkedAddress = checkAddressExists unvalidatedAddress
        // extract the innter value using pattern matching
        let (CheckedAddress checkedAddress) = checkedAddress

        let addressLine1 =
            checkedAddress.AddressLine1 |> String50.create
        let addressLine2 =
            checkedAddress.AddressLine2 |> String50.createOption
        let addressLine3 =
            checkedAddress.AddressLine3 |> String50.createOption
        let addressLine4 =
            checkedAddress.AddressLine4 |> String50.createOption
        let city =
            checkedAddress.City |> String50.create
        let zipCode =
            checkedAddress.ZipCode |> ZipCode.create
        // create the address
        let address : Address = {
            AddressLine1 = addressLine1
            AddressLine2 = addressLine2
            AddressLine3 = addressLine3
            AddressLine4 = addressLine4
            City = city
            ZipCode = zipCode }
        // return the address
        address

    let validateOrder : ValidateOrder =
        fun checkProductCodeExists checkAddressExists unvalidatedOrder ->
            
            let orderId =
                unvalidatedOrder.OrderId
                |> OrderId.create

            let customerInfo =
                unvalidatedOrder.CustomerInfo
                |> toCustomerInfo

            let shippingAddress =
                unvalidatedOrder.ShippingAddress
                |> toAddress checkAddressExists

            { OrderId = orderId
              CustomerInfo = customerInfo
              ShippingAddress = shippingAddress }
