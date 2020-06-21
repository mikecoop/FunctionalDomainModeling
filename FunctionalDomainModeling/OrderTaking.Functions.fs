module OrderTaking.Functions

    open OrderTaking.Domain

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

    let toOrderQuantity productCode quantity =
        let orderQuantity =
            match productCode with
            | Widget _ ->
                quantity
                |> int
                |> UnitQuantity.create
                |> Result.map Unit
            | Gizmo _ ->
                quantity
                |> KilogramQuantity.create
                |> Result.map Kilogram
        match orderQuantity with
        | Error e -> failwith e
        | Ok q -> q


    let predicateToPassThru errorMsg f x =
        if f x then
            x
        else
            failwith errorMsg

    let toProductCode (checkProductCodeExists:CheckProductCodeExists) productCode =
        let checkProduct productCode =
            let errorMsg = sprintf "Invalid %A" productCode
            predicateToPassThru errorMsg checkProductCodeExists productCode
        productCode
        |> ProductCode.create
        |> checkProduct

    let toValidatedOrderLine checkProductCodeExists
        (unvalidatedOrderLine:UnvalidatedOrderLine) =
        let orderLineId =
            unvalidatedOrderLine.OrderLineId
            |> OrderLineId.create
        let productCode =
            unvalidatedOrderLine.ProductCode
            |> toProductCode checkProductCodeExists
        let quantity =
            unvalidatedOrderLine.Quantity
            |> toOrderQuantity productCode
        let validatedOrderLine = {
            OrderLineId = orderLineId
            ProductCode = productCode
            Quantity = quantity }
        validatedOrderLine

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

            let billingAddress =
                unvalidatedOrder.BillingAddress
                |> toAddress checkAddressExists

            let orderLines =
                unvalidatedOrder.OrderLines
                |> List.map (toValidatedOrderLine checkProductCodeExists)

            { OrderId = orderId
              CustomerInfo = customerInfo
              ShippingAddress = shippingAddress
              BillingAddress = billingAddress
              Lines = orderLines }

    let toPricedOrderLine getProductPrice (line:ValidatedOrderLine) : PricedOrderLine =
        let qty = line.Quantity |> OrderQuantity.value
        let price = line.ProductCode |> getProductPrice
        let linePrice = price |> Price.multiply qty
        {
            OrderLineId = line.OrderLineId
            ProductCode = line.ProductCode
            Quantity = line.Quantity
            LinePrice = linePrice
        }

    let priceOrder : PriceOrder =
        fun getProductPrice (validatedOrder:ValidatedOrder) ->
            let lines : PricedOrderLine list =
                validatedOrder.Lines
                |> List.map (toPricedOrderLine getProductPrice)
            let amountToBill =
                lines
                |> List.map (fun line -> line.LinePrice)
                |> BillingAmount.sumPrices
            let pricedOrder : PricedOrder = {
                OrderId = validatedOrder.OrderId
                CustomerInfo = validatedOrder.CustomerInfo
                ShippingAddress = validatedOrder.ShippingAddress
                BillingAddress = validatedOrder.BillingAddress
                Lines = lines
                AmountToBill = amountToBill }
            pricedOrder
