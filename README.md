# 🏦 Bank Server
This is a toy bank server that implements the Actor Model with Erlang. 

## The Actor Model 
The **actor model** in computer science is a mathematical model of concurrent computation that treats an actor as the basic building block of concurrent computation. In response to a message it receives, an actor can: 
* make local decisions 
* create more actors
* send more messages
* determine how to respond to the next message received. 
  
Actors may modify their own private state, but can only affect each other indirectly through messaging.
You can read more about it [here](https://en.wikipedia.org/wiki/Actor_model).

In even simpler terms, by Joe Armstrong, co-designer of the Erlang:
![Joe Armstrong, co-designer of Erlang.](joe_armstrong_tweet.png)

## 🏦 The bank server
This bank server is a toy project created to implement a simple case of the actor model. It is based on an exercise from the course **Practical Concurrent and Parallel Programming** from the **IT University of Copenhagen**. Special thanks to the great professor Raúl Pardo Jimenez. 

## 🧱 Project Structure
| Module | Description |
|-------|-------------|
| `server` | Entry point of the application. Initializes the server and handles requests. It has an easy_start command that will create banks, accounts and mobile apps, or can be initialized manually. |
| `mobile_app` | Represents a user of the system. Each mobile app requires an account and a bank to be initialized. |
| `bank` | Represents a bank that keeps a record of all its accounts and which mobile app owns them. It receives payment requests from the mobile apps, and it is the gatekeeper for spurious requests |
| `account` | Represents a bank account. It prints and updates its balance |
---

## 📋 Rules
- A mobile app can only make payments from accounts it owns.  
- Unauthorized payment requests are rejected.  


## 📈 Possible Improvements
This simple project could be further improved with the following:
- Denying payments that would generate negative balances.
- Keeping a register in the server to prevent two mobile apps from using the same account.
- Allowing the creation of mobile apps without a bank and an account, that could later be added.
- Do you have an idea? Open an issue or PR!

## 🕹️ How to Run
1. Install Erlang https://www.erlang.org/downloads
2. Clone the repo.
3. Run `erlc *erl` to compile the modules.
4. Run `erl` to start the Erlang shell.
5. Run `server:start_reg(test).` to create a server.
6. You can run `test ! easy_start` to automatically create banks, accounts and mobile apps. Otherwise you can create them manually.

## 📋 Allowed server requests
With the server created as "test" (or any name you like), you can send requests to it using the Erlang shell. For that you need to "bang" your message to the server. It is simple, just run: `test ! YOUR-REQUEST.`  The possible requests are:
- `easy_start` starts the server with five banks (danske, jyske, al, nordea, lunar), six accounts (a1,a2,a3,a4,a5,a6) and six mobile apps (lisandro, henrik, peter, marie, signe, valdemar). The order given for the accounts and mobile apps, respects their ownership (eg. lisandro>a1, henrik>a2, etc). It can only run once, and it won't if a bank, account or mobile app were created manually.
-  `{add_bank, BankID}` adds a bank to the server if that name (BankID) doesn't exist yet.
-  `{add_account, AccountID}` adds an account to the server if that name (AccountID) doesn't exist yet.
-  `{add_mobileapp, AccountID, BankID, UserID}` adds a mobile app to the server if that name (UserID) doesn't exist yet. It requires a previously created account and bank.
-  `{make_payment, MobileAppSender, AccountIDSender, AccountIDReceiver, Amount}` requests a payment from a mobile app (MobileAppSender), that owns the AccountIDSender, to the AccountIDReceiver for the specified amount. eg. `{make_payment, lisandro, a1, a2, 20}`. If the MobileAppSender is not the owner of the AccountIDSender, the payment will be rejected by the bank.
-  `print_all_balances` prints the balances of all the mobile apps registered.
-  `print_banks_list` prints all the banks registered.
-  `print_accounts_list` prints all the accounts registered.
-  `print_mobileapp_list` prints all the mobile apps registered.




