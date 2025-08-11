# bank_app
This is a toy bank app that implements the Actor Model with Erlang. 

## The Actor Model 
The **actor model** in computer science is a mathematical model of concurrent computation that treats an actor as the basic building block of concurrent computation. In response to a message it receives, an actor can: 
* make local decisions 
* create more actors
* send more messages
* determine how to respond to the next message received. 
  
Actors may modify their own private state, but can only affect each other indirectly through messaging.
You can read this and more about it in [here](https://en.wikipedia.org/wiki/Actor_model).

In even simpler terms:
![Joe Armstrong, co-designer of the Erlang.](joe_armstrong_tweet.png)



