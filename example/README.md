[Main.elm](Main.elm) is a simple example of using the S3 library in the [billstclair/elm-dynamodb](http://package.elm-lang.org/packages/billstclair/elm-dynamodb/latest) package.

In order to use it, you'll need to get a secret key and access key. There are multiple possibilities for Amazon AWS, but you'll probably want to create keys and give access priveleges from the [Identity and Access Management (IAM)](https://console.aws.amazon.com/iam/) console. See the "Accounts File" section below for details.

You can run the example in Elm reactor:

    cd .../elm-dynamodb/example
    elm reactor
    
Then aim your web browser at http://localhost:8000, to see this file, and click on [Main.elm](Main.elm) to run the code.

# Accounts File

[accounts.json.template](accounts.json.template) is a template for a file which describes your DynamoDB accounts. Copy it to `accounts.json`, and edit to match your account(s). It is a list of JSON objects, each of which has the following properties:

`"name"` is a string specifying the name to appear in the "Account" selection.

`"region"` is the region for the buckets. If omitted, it will use the global region, which only works for global tables.

`"access-key"` is a 20-character string specifying the account access key. This goes over the wire.

`"secret-key"` is a 40-character string specifying the account secret key. This is used for signing.

`"tableName"` is a string naming the DynamoDB table to access.

For example:

    [{"name": "Amazon DynamoDB",
      "region": "us-east-1",
      "access-key": "<20-character access key>",
      "secret-key": "<40-character secret key>",
      "tableName": "<tableName>"
     }
    ]

Don't store the `accounts.json` file online anywhere, or you're likely to give away your secret key. Usually, your application will ask the user to enter access and secret keys, and you'll store them in `LocalStorage`, making them private to the user's browser.

# Test Table Restrictions

The AmazonDB table that you choose from `accounts.json` must have a simple primary key attribute with string values, or nothing but "Scan" will work. The "Key Name" defaults to "key", but you can type something different.
