hot_proxy
=====

An OTP application

Need to make the router lib use lrw to compute the node to route to based on weight, and cache the result
for the max lifetime of the session.

Goal is to make it so that you can say "Route to node A twice as often as your route to node B, and clients should stick to their node
for X period of time after being assigned".

The objective is to make sure that nodes get hit proportionally, but so that clients don't potentially have bouncing sessions dependent on how big
the difference between the proxied environments is.


Need to look into re-working the event implementation to use an ets backed trie.  Shouldn't be too hard to implement, just need to snipe a bit of
the logic out of the js trie implementation.

Build
-----

    $ rebar3 compile
