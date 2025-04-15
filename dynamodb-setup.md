# Setting up a DynamoDB table

This file is a simple tutorial on creating a DynamoDB table in Amazon AWS.

## Create an AWS root account and a DynamoDB user account

Aim your browser at [aws.amazon.com](https://aws.amazon.com/). If you already have an account, log in. Otherwise, click the "Create an AWS Account" button, and go through the process.

Go to the [AWS IAM dashboard](https://console.aws.amazon.com/iamv2/), click on "Users" in the left column, then the blue "Add Users" button.

Enter one or "User name"s, and "Select AWS credential type". Click the blue "Next: Permissions" button.

Click "Attach existing policies directly".

You may want "AmazonDynamoDBFullAccess" (type "dynamo" in the "Filter policies" input area to find it). I created my own custom policy with access to only the DynamoDB actions I needed (BatchGetItem, GetItem, Query, Scan, BatchWriteItem, DeleteItem, PutItem, and UpdateItem). This was enough to make the example application work.

Click the blue "Next: Tags" button. Add tags if you want them.

Click the blue "Next: Review" button.

Click the blue "Create User" button.

Click the "Download .csv" button, or write down the "Access Key ID" and "Secret access key. If you forget the "Secret access key", Amazon will never again tell it to you, but you can generate a new one.

## Create a DynamoDB table

Go to the [DynamoDB Dashboard](https://console.aws.amazon.com/dynamodbv2/).

Click on "Tables" in the left column.

Click the orange "Create table" button.

Enter a "Table name". This name need not be unique to the world. You have your own namespace for tables.

Enter a "Partition key" name, and choose its type.

If you want a "Sort key", enter that and choose its type. The example code assumes there is NOT a sort key, but the DynamoDB module supports it.

Usually you can use "Default settings" and need no tags. You can edit these later, if need be.

Click the orange "Create table" button.

It will take a few seconds to create the table. It will have a link on the "Tables" page when it's ready.

Set the "Capacity Mode", high enough for your application. I use "On-demand".

There's a lot you can do in the AWS console, but you can often just write code (or use the example UI).
