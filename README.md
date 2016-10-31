emq_mod_subscription
====================

Subscription Management Module

Configuration
-------------

etc/emq_mod_subscription.conf

```
## Subscribe the Topics automatically when client connected
module.subscription.1.topic = $client/%c
## Qos of the subscription: 0 | 1 | 2
module.subscription.1.qos = 1

##module.subscription.2.topic = $user/%u
##module.subscription.2.qos = 1

## Load static subscriptions from backend storage
## Values: on | off
module.subscription.backend = on
```

License
-------

Apache License Version 2.0

Author
------

feng at emqtt.io

