[![elm-package](https://img.shields.io/badge/elm-2.0.0-blue.svg)](http://package.elm-lang.org/packages/billstclair/elm-dynamodb/latest)

A pure-Elm client for the [Amazon DynamoDB](https://aws.amazon.com/dynamodb/) NoSQL database service. It targets a subset of the service API. Use `DynamoDB.makeRequest` to add more.

AWS Documentation: https://docs.aws.amazon.com/dynamodb

AWS Console: https://console.aws.amazon.com/dynamodbv2

[dynamodb-setup.md](https://github.com/billstclair/elm-dynamodb/blob/main/dynamodb-setup.md) contains a simple tutorial for creating AWS root and user accounts and your first DynamoDB table.

# Example

The [`example`](https://github.com/billstclair/elm-dynamodb/tree/master/example) directory has some sample code, with a simple user interface.

# Credits

My thanks to Kevin Tonon for his `elm-aws-core` package, and to the-sett for upgrading it to Elm 0.19.1: [the-sett/elm-aws-core](http://package.elm-lang.org/packages/the-sett/elm-aws-core/latest) package. Without it, I would likely have thrown up my hands in despair over ever getting the signing and authorization crypto to work.
